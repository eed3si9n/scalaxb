/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler.xsd

import scalashim._
import scalaxb.compiler.{Config, Snippet, CaseClassTooLong, Log}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._

abstract class GenSource(val schema: SchemaDecl,
    val context: XsdContext) extends Parsers with XMLOutput {
  private val logger = Log.forName("xsd.GenSource")
  type =>?[A, B] = PartialFunction[A, B]
  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val MIXED_PARAM = "mixed"

  def run: Snippet = {
    logger.debug("run")
    
    val snippets = mutable.ListBuffer.empty[Snippet]
    snippets += Snippet(makeSchemaComment, Nil, Nil, Nil)

    schema.typeList map {
      case decl: ComplexTypeDecl if !context.duplicatedTypes.contains((schema, decl)) =>
        if (context.baseToSubs.contains(decl)) {
          snippets += makeTrait(decl)
          if (!decl.abstractValue) snippets += makeSuperType(decl)
        }
        else snippets += makeType(decl)
      case decl: SimpleTypeDecl if !context.duplicatedTypes.contains((schema, decl)) =>
        if (containsEnumeration(decl)) snippets += makeEnumType(decl)
      case _ =>
    }
    
    for ((sch, group) <- context.groups if sch == this.schema)
      snippets += makeGroup(group)
    for (group <- schema.topAttrGroups.valuesIterator)
      snippets += makeAttributeGroup(group)
    
    Snippet(snippets: _*)
  }
    
  def makeSuperType(decl: ComplexTypeDecl): Snippet = {
    val localName = makeProtectedTypeName(schema.targetNamespace, decl, context)
    val fqn = buildFullyQualifiedNameFromNS(schema.targetNamespace, localName)
    makeCaseClassWithType(localName, fqn, decl)
  }
    
  def makeType(decl: ComplexTypeDecl): Snippet =
    makeCaseClassWithType(buildTypeName(decl, true), buildTypeName(decl, false), decl)
  
  def types(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topTypes.contains(name))
      yield schema.topTypes(name)) match {
        case x :: xs => x
        case Nil     => sys.error("Type not found: {" + namespace + "}:" + name)
      }
      
  def baseToDescendants(base: ComplexTypeDecl): List[ComplexTypeDecl] =
    context.baseToSubs(base) flatMap { child =>
      child :: (
        if (context.baseToSubs.contains(child)) baseToDescendants(child)
        else Nil
      )
    }

  def makeTrait(decl: ComplexTypeDecl): Snippet = {
    val localName = buildTypeName(decl, true)
    val fqn = buildTypeName(decl, false)
    val formatterName = buildFormatterName(decl.namespace, localName)
    logger.debug("makeTrait: emitting " + fqn)
    val effectiveMixed = buildEffectiveMixed(decl)    
    val childElements = if (effectiveMixed) Nil
      else flattenElements(decl)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list map { buildParam }
    val defaultType = buildFullyQualifiedNameFromNS(schema.targetNamespace, makeProtectedTypeName(schema.targetNamespace, decl, context))    
    val argList = list map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl, false)
      case x => buildArg(x)
    }
    val superNames = buildSuperNames(decl)
    
    val extendString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) =
      "case (" + quoteNamespace(decl.namespace) + ", " + quote(Some(decl.family.head)) + ") => " +
        "Right(" + buildFromXML(buildTypeName(decl, false), "node", Some("stack"), None) + ")"
    
    def makeToXmlCaseEntry(decl: ComplexTypeDecl) =
      "case x: " + buildTypeName(decl, false) + " => " + 
        buildToXML(buildTypeName(decl, false), "x, __namespace, __elementLabel, __scope, true")
        
    val compositors = context.compositorParents.filter(_._2 == decl).keysIterator.toList
    // val extendedSubtypes = context.baseToSubs(decl) filter { sub =>
    //   !schema.typeList.contains(sub) && !dependentSchemas.exists(_.typeList.contains(sub)) }
    // val extendedSchemas = (for (sub <- extendedSubtypes;
    //                             sch <- context.schemas; if sch.typeList.contains(sub))
    //                          yield sch).toList.distinct
    // val imports = extendedSchemas map { sch =>
    //   val pkg = packageName(sch, context)
    //   val name = context.typeNames(pkg)(sch)
    //   "import " + pkg.map(_ + ".").getOrElse("") + buildDefaultProtocolName(name) + "._"
    // }  
    
    val traitCode = <source>{ buildComment(decl) }trait {localName}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param.toTraitScalaCode
  vals.mkString(newline + indent(1))}
}}</source>
    
    val compDepth = 1
    val defaultFormats = <source>  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    { // if (imports.isEmpty) ""
      //  else imports.mkString(newline + indent(2)) + newline + indent(2) 
    }def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node =>     
        scalaxb.Helper.instanceType(node) match {{
          { val cases = for (sub <- baseToDescendants(decl) if sub.isNamed)
              yield makeCaseEntry(sub)
            cases.mkString(newline + indent(4 + compDepth))        
          }
          { if (!decl.abstractValue) "case _ => Right(" + buildFromXML(defaultType, "node", Some("stack"), None) + ")"
            else """case x => Left("Unknown type: " + x)""" }
        }}
      case _ => Left("reads failed: seq must be scala.xml.Node")  
    }}
    
    def writes(__obj: {fqn}, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq = __obj match {{
      { val cases = for (sub <- context.baseToSubs(decl))
          yield makeToXmlCaseEntry(sub)
        cases.mkString(newline + indent(2 + compDepth))        
      }
      { if (!decl.abstractValue) "case x: " + defaultType + " => " +
          buildToXML(defaultType, "x, __namespace, __elementLabel, __scope, false")
        else """case _ => sys.error("Unknown type: " + __obj)"""
      }
    }}
  }}</source>
    
    val compositorCodes: Seq[Snippet] = if (decl.abstractValue) compositors map { makeCompositor }
      else Nil
    
    Snippet(Snippet(traitCode, <source/>, defaultFormats, makeImplicitValue(fqn, formatterName)) +:
      compositorCodes: _*)
  }
  
  def makeImplicitValue(fqn: String, formatterName: String): Node =
    <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{fqn}] = new Default{formatterName} {{}}</source>
  
  def makeImplicitValue(group: AttributeGroupDecl): Node = {
    val formatterName = buildFormatterName(group)
    val fqn = buildTypeName(group, false)
    <source>  implicit lazy val {formatterName}: scalaxb.AttributeGroupFormat[{fqn}] = new Default{formatterName} {{}}</source>
  }

  def isQualifyAsIRIStyle(decl: ComplexTypeDecl): Boolean = {
    val primary = decl.content match {
      case ComplexContentDecl(CompContRestrictionDecl(_, x, _)) => x
      case ComplexContentDecl(CompContExtensionDecl(_, x, _)) => x
      case _ => None
    }
    primary match {
      case Some(SequenceDecl(_, _, _, _, _)) =>
        val flatParticles = flattenElements(decl)
        val attributes = flattenAttributes(decl)
        flatParticles.forall(_.typeSymbol match {
          case AnyType(symbol) => false
          case symbol: BuiltInSimpleTypeSymbol => true
          case ReferenceTypeSymbol(decl: SimpleTypeDecl) => true
          case _ => false
        }) && attributes.isEmpty
      case _ => false
    }
  }

  def makeCaseClassWithType(localName: String, fqn: String, decl: ComplexTypeDecl): Snippet = {
    logger.debug("makeCaseClassWithType: emitting " + fqn)
    val formatterName = buildFormatterName(decl.namespace, localName)

    val primary = decl.content match {
      case ComplexContentDecl(CompContRestrictionDecl(_, x, _)) => x
      case ComplexContentDecl(CompContExtensionDecl(_, x, _)) => x
      case _ => None
    }
    val effectiveMixed = buildEffectiveMixed(decl)
    val superNames: List[String] =
      if (context.baseToSubs.contains(decl)) List(buildTypeName(decl, true))
      else buildSuperNames(decl)

    val flatParticles = flattenElements(decl)
    // val particles = buildParticles(decl, name)
    val childElements = if (effectiveMixed) flattenMixed(decl)
      else flatParticles 
    val attributes = flattenAttributes(decl)
    val longAttribute = (!namedAttributes && !attributes.isEmpty) ||
      (attributes.size + childElements.size > contentsSizeLimit &&
      childElements.size + 1 <= contentsSizeLimit)
    val list = if (longAttribute) List.concat[Decl](childElements, List(buildLongAttributeRef))
      else List.concat[Decl](childElements, attributes)
    val paramList = list map { buildParam }
    // val dependents = ((flatParticles flatMap { buildDependentType } collect {
    //   case ReferenceTypeSymbol(d: ComplexTypeDecl) if d != decl => d
    // }).toList ++ (attributes collect {
    //   case group: AttributeGroupDecl => group
    // }).toList).distinct
    
    val unmixedParserList = flatParticles map { buildParser(_, effectiveMixed, effectiveMixed, false) }
    val parserList = if (effectiveMixed) buildTextParser :: (unmixedParserList flatMap { List(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList = ( 0 to parserList.size - 1) map { buildSelector }
    
    val longAll: Boolean = primary match {
        case Some(all: AllDecl) if isLongAll(all, decl.namespace, decl.family) => true
        case _ => false
      }
    val particleArgs = if (effectiveMixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(flatParticles((i - 1) / 2), i, false)
        else buildArgForOptTextRecord(i) }
      else primary match {
        case Some(all: AllDecl) => all.particles map { buildArgForAll(_, longAll) }
        case _ => (0 to flatParticles.size - 1).toList map { i => buildArg(flatParticles(i), i) }
      }
    
    val accessors = (primary match {
        case Some(all: AllDecl) if longAll => generateAccessors(all)
        case _ => generateAccessors(paramList, splitSequences(decl))
      }) ::: (if (longAttribute) generateAccessors(attributes) else Nil)
    logger.debug("makeCaseClassWithType: generateAccessors " + accessors)

    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator.toList
        
    val extendString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) && (!effectiveMixed) && (!longAll) && (config.varArg)
    
    def paramsString = if (hasSequenceParam) makeParamName(paramList.head.name, false) + ": " +
                                              paramList.head.singleTypeName + "*"

                       else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))

    val simpleFromXml: Boolean = if (flatParticles.isEmpty && !effectiveMixed) true
      else (decl.content, primary) match {
        case (x: SimpleContentDecl, _) => true
        case (_, Some(all: AllDecl)) => true
        case _ => false
      }
    
    def argsString = if (hasSequenceParam) particleArgs.head + ": _*"
      else {
        val particleString = if (effectiveMixed) "Seq.concat(" + particleArgs.mkString("," + newline + indent(4)) + ")"
          else if (longAll) "scala.collection.immutable.ListMap(List(" + newline + 
              indent(4) + particleArgs.mkString("," + newline + indent(4)) + ").flatten[(String, scalaxb.DataRecord[Any])]: _*)"
          else decl.content match {
            case simp@SimpleContentDecl(SimpContRestrictionDecl(base: XsTypeSymbol, _, _, _)) =>
              buildArg(simp, base)      
            case simp@SimpleContentDecl(SimpContExtensionDecl(base: XsTypeSymbol, _)) =>
              buildArg(simp, base)
            case _ => particleArgs.mkString("," + newline + indent(4))
          }        
        val attributeString = if (attributes.isEmpty) ""
          else {
            val notAnyAttributes = attributes filter { 
              case any: AnyAttributeDecl => false
              case _ => true
            }
            val anyAttributes = attributes filter {
              case any: AnyAttributeDecl => true
              case _ => false
            }            
            
            if (longAttribute) {
              val nonAnyString = if (notAnyAttributes.isEmpty) ""
                else "List(" + newline + 
                  indent(4) + (notAnyAttributes map { x => 
                    buildArgForAttribute(x, Some("node"), longAttribute) }).mkString("," + newline + indent(4)) + newline +
                  indent(4)  + ").flatten[(String, scalaxb.DataRecord[Any])]"
              val anyString = if (anyAttributes.isEmpty) ""
                else "(" + buildArgForAnyAttribute(decl, longAttribute) + ")"
              "scala.collection.immutable.ListMap(" + 
                (if (nonAnyString != "" && anyString != "") nonAnyString + " ::: " + anyString
                 else nonAnyString + anyString ) + ": _*)"
            } 
            else (attributes map {
                    case any: AnyAttributeDecl => buildArgForAnyAttribute(decl, longAttribute)
                    case x => buildArgForAttribute(x, Some("node"), longAttribute) 
                 }).mkString("," + newline + indent(4))
          } // if-else
          
        if (!particleString.isEmpty && !attributeString.isEmpty) particleString + "," + newline +
          indent(4) + attributeString
        else particleString + attributeString
      }
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def makeWritesChildNodes = {
      def simpleContentString(base: XsTypeSymbol) = base match {
        case AnyType(symbol) => 
          "__obj.value.value match {" + newline +
          indent(4) + "case elem: scala.xml.Elem => elem.child" + newline +
          indent(4) + "case _ => Seq(scala.xml.Text(__obj.value.value.toString))" + newline +
          indent(3) + "}"
        case _ => "Seq(scala.xml.Text(__obj.value.toString))"
      }

      def childString(decl: ComplexTypeDecl): String =
        if (effectiveMixed) "__obj." + makeParamName(MIXED_PARAM, false) +
          ".toSeq flatMap { x => " + buildToXML("scalaxb.DataRecord[Any]", "x, x.namespace, x.key, __scope, false") + " }"
        else decl.content.content match {
          case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) => childString(base)
          case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) => childString(base)
          case SimpContRestrictionDecl(base: XsTypeSymbol, _, _, _) => simpleContentString(base)
          case SimpContExtensionDecl(base: XsTypeSymbol, _)         => simpleContentString(base)
          case _ =>
            if (childElemParams.isEmpty) "Nil"
            else if (childElemParams.size == 1) "(" + buildXMLString(childElemParams(0)) + ")"
            else childElemParams.map(x =>
              buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
        }

      <source>    def writesChildNodes(__obj: {fqn}, __scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node] =
      {childString(decl)}</source>
    }

    val groups = filterGroup(decl).distinct filter { g => primaryCompositor(g).particles.size > 0 }
    val defaultFormatSuperNames: List[String] = "scalaxb.ElemNameParser[" + fqn + "]" :: groups.map(g =>
      buildFormatterName(g.namespace, groupTypeName(g))).distinct
    
    val caseClassCode = <source>{ buildComment(decl) }case class {localName}({paramsString}){extendString}{ if (accessors.size == 0) ""
      else " {" + newline +
        indent(1) + accessors.mkString(newline + indent(1)) + newline +
        "}" + newline}
      </source>

    def defaultFormats = if (simpleFromXml) <source>  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] with scalaxb.CanWriteChildNodes[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    import scalaxb.ElemName._
    
    def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node => Right({fqn}({argsString}))
      case _ => Left("reads failed: seq must be scala.xml.Node")
    }}
    
{makeWritesAttribute}{makeWritesChildNodes}
  }}</source>
    else <source>  trait Default{formatterName} extends {defaultFormatSuperNames.mkString(" with ")} {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    { if (decl.isNamed) "override def typeName: Option[String] = Some(" + quote(decl.name) + ")" + newline + newline + indent(2)  
      else ""
    }{ if (effectiveMixed) "override def isMixed: Boolean = true" + newline + newline + indent(2)
       else "" }def parser(node: scala.xml.Node, stack: List[scalaxb.ElemName]): Parser[{fqn}] =
      {phrase}({ parserList.mkString(" " + follow + " " + newline + indent(3)) } ^^
      {{ case { parserVariableList.mkString(s" $follow ") } =>
      {fqn}({argsString}) }})
    
{makeWritesAttribute}{makeWritesChildNodes}  }}</source>
    
    def makeWritesAttribute = if (attributes.isEmpty) <source></source>
      else if (longAttribute) {
        val cases = attributes collect {
          case attr: AttributeDecl => "case (" + quote(buildNodeName(attr, false)) + ", _) => " + buildAttributeString(attr)
          case ref: AttributeRef =>
            val attr = buildAttribute(ref)
            "case (" + quote(buildNodeName(attr, false)) + ", _) => " + buildAttributeString(attr)
          case group: AttributeGroupDecl => "case (" + quote(buildNodeName(group)) + ", _) => " + buildAttributeString(group)
        }
        val caseString = if (cases.isEmpty) ""
          else cases.mkString(newline + indent(4)) + newline + indent(4)
        <source>    override def writesAttribute(__obj: {fqn}, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = {{
      var attr: scala.xml.MetaData  = scala.xml.Null
      __obj.{makeParamName(ATTRS_PARAM, false)}.toList map {{
        {caseString}case (key, x) => attr = scala.xml.Attribute((x.namespace map {{ __scope.getPrefix(_) }}).orNull, x.key.orNull, x.value.toString, attr)
      }}
      attr
    }}</source>
      } else <source>    override def writesAttribute(__obj: {fqn}, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = {{
      var attr: scala.xml.MetaData  = scala.xml.Null
      { attributes.map(x => buildAttributeString(x)).mkString(newline + indent(3)) }
      attr
    }}</source>
    
    val compositorCodes = compositors map { makeCompositor }

    Snippet(Snippet(caseClassCode, <source/>, defaultFormats, makeImplicitValue(fqn, formatterName)) +:
      compositorCodes: _*)
  }
    
  def buildComment(p: Product) = p match {
    case decl: TypeDecl =>
      if (schema.typeToAnnotatable.contains(decl))
        makeAnnotation(schema.typeToAnnotatable(decl).annotation) + newline
      else makeAnnotation(decl.annotation) + newline
    case anno: Annotatable =>
      makeAnnotation(anno.annotation) + newline
    case _ => ""
  }
  
  // family is used to split sequences.
  def makeCompositor(compositor: HasParticle): Snippet = compositor match {
    case seq: SequenceDecl  => makeSequence(seq)
    case _ => 
      val superNames: List[String] = buildOptions(compositor)
      val superString = if (superNames.isEmpty) ""
        else " extends " + superNames.mkString(" with ")
      val localName = makeTypeName(context.compositorNames(compositor))
      Snippet(<source>trait {localName}{superString}</source>)
  }
  
  def makeSequence(seq: SequenceDecl): Snippet = {    
    val localName = makeTypeName(context.compositorNames(seq))
    val fqn = buildFullyQualifiedNameFromNS(schema.targetNamespace, localName)
    val formatterName = buildFormatterName(schema.targetNamespace, localName)
    logger.debug("makeSequence: emitting " + fqn)

    // pass in local name for the family.
    // since the sequence is already split at this point, it does not require resplitting.
    val particles = flattenElements(schema.targetNamespace, List(localName), seq, 0, false)
    val paramList = particles map { buildParam }

    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute)
    val paramsString = if (hasSequenceParam)
        makeParamName(paramList.head.name, false) + ": " + paramList.head.singleTypeName + "*"
      else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))    
    def makeWritesXML = <source>    def writes(__obj: {fqn}, __namespace: Option[String], __elementLabel: Option[String], 
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      {childString}</source>
    def childString = if (paramList.isEmpty) "Nil"
      else if (paramList.size == 1) buildXMLString(paramList(0))
      else paramList.map(x => 
        buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
    val superNames: List[String] = buildOptions(seq)
    val superString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    Snippet(<source>{ buildComment(seq) }case class {localName}({paramsString}){superString}</source>,
      <source/>,
      <source>  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, {fqn}] = Left("don't call me.")
    
{makeWritesXML}
  }}</source>,
      makeImplicitValue(fqn, formatterName))
  }
    
  def makeGroup(group: GroupDecl): Snippet = {
    val compositors = context.compositorParents.filter(
      x => x._2 == makeGroupComplexType(group)).keysIterator.toList
    val localName = makeTypeName(context.compositorNames(group))
    val fqn = buildFullyQualifiedNameFromNS(schema.targetNamespace, localName)
    val formatterName = buildFormatterName(group.namespace, localName)
    logger.debug("makeGroup: emitting " + fqn)

    val compositor = primaryCompositor(group)
    val param = buildParam(compositor)
    val o = buildOccurrence(compositor).toSingle
    val wrapperParam = compositor match {
      case choice: ChoiceDecl => param
      case _ => param.copy(typeSymbol = XsDataRecord(param.typeSymbol))
    }
    val mixedParam = param.copy(typeSymbol = XsDataRecord(XsAnyType))
    val parser = buildCompositorParser(compositor, o, false, false, false)
    val wrapperParser = compositor match {
      case choice: ChoiceDecl => parser
      case _ => buildCompositorParser(compositor, o, false, true, false)
    }
    val mixedparser = buildCompositorParser(compositor, o, true, true, false)
    
    val groups = filterGroup(compositor).distinct
    val superNames: List[String] = 
      if (groups.isEmpty) List("scalaxb.AnyElemNameParser")
      else groups.map { g => buildFormatterName(g.namespace, groupTypeName(g)) }
    
    val defaultFormats = if (compositor.particles.size == 0) <source></source>
      else <source>{ buildComment(group) }  trait {formatterName} extends {superNames.mkString(" with ")} {{
    def parse{localName}(node: scala.xml.Node, stack: List[scalaxb.ElemName]): Parser[{param.baseTypeName}] =
      {parser}
  
    def parse{localName}(node: scala.xml.Node, stack: List[scalaxb.ElemName], wrap: Boolean): Parser[{wrapperParam.baseTypeName}] =
      {wrapperParser}
    
    def parsemixed{localName}(node: scala.xml.Node, stack: List[scalaxb.ElemName]): Parser[Seq[{mixedParam.baseTypeName}]] =
      {mixedparser}
  }}</source>
    
    val compositorCodes = compositors map { makeCompositor }
    Snippet(Snippet(Nil, Nil, defaultFormats, Nil) +:
      compositorCodes: _*)
  }
  
  def makeAttributeGroup(group: AttributeGroupDecl): Snippet = {
    val localName = buildTypeName(group, true)
    val fqn = buildTypeName(group, false)
    val formatterName = buildFormatterName(group.namespace, localName)
    logger.debug("makeAttributeGroup: emitting " + fqn)

    val attributes = flattenAttributes(group.attributes)
    val paramList = attributes map { buildParam }
    val argList = attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(group, false)
        case x => buildArg(x) 
      }
    val paramsString = paramList.map(
      _.toScalaCode).mkString("," + newline + indent(1))
    val argsString = argList.mkString("," + newline + indent(3))  
    val attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    
    val caseClassCode = <source>{ buildComment(group) }case class {localName}({paramsString})</source>
    val defaultFormats = <source>  trait Default{formatterName} extends scalaxb.AttributeGroupFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node => Right({fqn}({argsString}))
      case _ => Left("reads failed: seq must be scala.xml.Node")
    }}
    
    def toAttribute(__obj: {fqn}, __attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = {{
      var attr: scala.xml.MetaData  = __attr
      {attributeString}
      attr
    }}
  }}</source>
    
    Snippet(caseClassCode,
      <source/>,
      defaultFormats,
      makeImplicitValue(group))
  }
  
  def makeEnumType(decl: SimpleTypeDecl) = {
    val localName = buildTypeName(decl, true)
    val fqn = buildTypeName(decl, false)
    val formatterName = buildFormatterName(decl.namespace, localName)
    val enums = filterEnumeration(decl).distinct

    val baseSym : Option[XsTypeSymbol] = decl.content match {case SimpTypRestrictionDecl(base, _) => Some(base) case _ => None}
    val baseType: Option[String      ] = baseSym.map(buildTypeName(_))

    def makeEnum(enum: EnumerationDecl[_]) =
      "case object " + buildTypeName(localName, enum, true) + " extends " + localName + 
      " { override def toString = " + quote(enum.value.toString) + " }"
    
    def makeCaseEntry(enum: EnumerationDecl[_]) = baseSym match {
      case Some(XsQName) => s"${indent(3)}case ${quote(enum.value.toString)} => ${buildTypeName(localName, enum, true)}\n"
      case _ => baseType.map {tpe =>
        s"${indent(3)}case x: $tpe if x == scalaxb.fromXML[$tpe](scala.xml.Text(${quote(enum.value.toString)})) => ${buildTypeName(localName, enum, true)}\n"
      }.getOrElse {
        s"${indent(3)}case ${quote(enum.value.toString)} => ${buildTypeName(localName, enum, true)}\n" 
      }
    }

    
    val enumString = enums.map(makeEnum).mkString(newline)

    def valueCode: String = baseSym match {
        case Some(XsQName) => """({ val (ns, localPart) = scalaxb.Helper.splitQName(value, scope)
    new javax.xml.namespace.QName(ns.orNull, localPart).toString })"""
        case _ => baseType.map(tpe => s"scalaxb.fromXML[$tpe](scala.xml.Text(value))").getOrElse("value")
    }

    val traitCode = enums match {
      case Nil =>
<source>case class {localName}()

object {localName} {{
  def fromString(value: String, scope: scala.xml.NamespaceBinding): {localName} = {localName}()
}}</source>    
      case _ =>
<source>trait {localName}

object {localName} {{
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[{fqn}]): {localName} = fmt.reads(scala.xml.Text(value), Nil) match {{
    case Right(x: {localName}) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }}
}}

{ enumString }</source>
    }  // match
        
    Snippet(traitCode,
      Nil,
      <source>  def build{formatterName} = new Default{formatterName} {{}}
  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def fromString(value: String, scope: scala.xml.NamespaceBinding): {localName} = {valueCode} match {{
{ enums.map(e => makeCaseEntry(e)) }
    }}

    def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, {fqn}] = seq match {{
      case elem: scala.xml.Elem => Right(fromString(elem.text, elem.scope))
      case _ => Right(fromString(seq.text, scala.xml.TopScope))
    }}
    
    def writes(__obj: {fqn}, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      scala.xml.Elem(scalaxb.Helper.getPrefix(__namespace, __scope).orNull, 
        __elementLabel getOrElse {{ sys.error("missing element label.") }},
        scala.xml.Null, __scope, true, scala.xml.Text(__obj.toString))
  }}</source>,
      makeImplicitValue(fqn, formatterName))
  }

  def buildEffectiveMixed(decl: ComplexTypeDecl): Boolean =
    decl.content match {
      case x: ComplexContentDecl if decl.mixed => true
      case x: SimpleContentDecl                => false
      case x: ComplexContentDecl =>
        x.content.base match {
          case ReferenceTypeSymbol(base: ComplexTypeDecl) => buildEffectiveMixed(base)
          case _ => false
        }
    }
    
  def buildSuperNames(decl: ComplexTypeDecl) =
    buildSuperName(decl) ::: buildOptions(decl)
  
  def buildSuperName(decl: ComplexTypeDecl) = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => List(buildTypeName(base, true))
      case _ => Nil
    }

  def buildOptions(decl: ComplexTypeDecl): List[String] = {
    val set = mutable.ListBuffer.empty[String]
    def addIfMatch(typeSymbol: XsTypeSymbol, choice: ChoiceDecl) = {
      typeSymbol match {
        case ReferenceTypeSymbol(that: ComplexTypeDecl) =>
          if (that.namespace == decl.namespace &&
              that.name == decl.name &&
              !containsForeignType(choice))
            set += makeTypeName(context.compositorNames(choice))
        case _ => 
      }
    }
    
    for (sch <- context.schemas;
        choice <- sch.choices;
        particle <- choice.particles) particle match {
      case elem: ElemDecl => addIfMatch(elem.typeSymbol, choice)
      case ref: ElemRef   => addIfMatch(buildElement(ref).typeSymbol, choice)      
      case _ => // do nothing
    }
        
    set.toList.distinct
  }
  
  // reverse lookup all choices that contains that.
  def buildOptions(that: HasParticle): List[String] = {
    val set = mutable.ListBuffer.empty[String]
    
    def addIfMatch(comp: HasParticle, choice: ChoiceDecl) {
      if (comp == that && !containsForeignType(choice))
        set += makeTypeName(context.compositorNames(choice))     
    }
    
    def addIfContains(choice: ChoiceDecl) {
      choice.particles foreach { _ match {
          case ch: ChoiceDecl =>
            addIfMatch(ch, choice)
            addIfContains(ch)
          case comp: HasParticle => addIfMatch(comp, choice)
          case _ =>
        }
      }
    }

    for (sch <- context.schemas;
        choice <- sch.choices)
      addIfContains(choice)
    set.toList.distinct
  }
  
  def filterGroup(decl: ComplexTypeDecl): List[GroupDecl] = decl.content.content match {
    // complex content means 1. has child elements 2. has attributes
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      filterGroup(base)        
    case res@CompContRestrictionDecl(XsAnyType, _, _) =>
      filterGroup(res.compositor)
    
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      filterGroup(base) :::
        filterGroup(ext.compositor)
    case ext@CompContExtensionDecl(XsAnyType, _, _) =>
      filterGroup(ext.compositor)
      
    case _ => Nil    
  }

  def filterGroup(compositor: Option[HasParticle]): List[GroupDecl] = compositor match {
    case Some(c) => filterGroup(c)
    case None => Nil
  }
  
  def filterGroup(compositor: HasParticle): List[GroupDecl] = compositor match {
    case ref: GroupRef    => List(buildGroup(ref))
    case group: GroupDecl => List(group)
    case _ =>
      (compositor.particles flatMap {
        case ref: GroupRef    => List(buildGroup(ref))
        case group: GroupDecl => List(group)
        case compositor2: HasParticle => filterGroup(compositor2)
        case _ => Nil
      })
  }
  
  def argSize(decl: ComplexTypeDecl): Int = decl.content.content match {
    // complex content means 1. has child elements 2. has attributes
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      argSize(base)
    case res@CompContRestrictionDecl(XsAnyType, _, _) =>
      argSize(res.compositor)
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      argSize(base) + argSize(ext.compositor)
    case ext@CompContExtensionDecl(XsAnyType, _, _) =>
      argSize(ext.compositor)
    case _ => 1 
  }

  def argSize(compositor: Option[HasParticle]): Int = compositor match {
    case Some(c) =>
      c match {
        case seq: SequenceDecl => c.particles.size
        case _ => 1
      }
    case None => 1
  }

  def flattenElements(decl: ComplexTypeDecl): List[ElemDecl] = {
    val index = decl.content.content match {
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) => argSize(base)
      case res@CompContRestrictionDecl(XsAnyType, _, _) => 0
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) => argSize(base)
      case ext@CompContExtensionDecl(XsAnyType, _, _) => 0
      case _ => 0    
    }

    anyNumbers.clear()

    val build: ComplexTypeContent =>? List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) =>
        flattenElements(base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base)
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base)        
      case res@CompContRestrictionDecl(XsAnyType, _, _) =>
        res.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil }
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base) :::
          (ext.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil })
      case ext@CompContExtensionDecl(XsAnyType, _, _) =>
        ext.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil }
      case _ => Nil
    }
    
    val pf = buildSimpleTypeRef orElse build
    pf(decl.content.content)
  }
  
  // sometimes we don't have ComplexTypeDecl because it's a group.
  def splitLongSequence(namespace: Option[String], family: List[String], particles: List[Particle]): List[Particle] =
    if (particles.size <= contentsSizeLimit && !isWrapped(namespace, family)) particles
    else splitLong[SequenceDecl](particles) { SequenceDecl(namespace, _, 1, 1, 0) }
  
  // used to generte accessor
  def splitSequences(decl: ComplexTypeDecl): List[SequenceDecl] = decl.content.content match {
    case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) => splitSequences(base)
    case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) => splitSequences(base)

    // complex content means 1. has child elements 2. has attributes
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) => splitSequences(base)        
    case res@CompContRestrictionDecl(XsAnyType, _, _) =>
      res.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil }
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      splitSequences(base) :::
        (ext.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil })
    case ext@CompContExtensionDecl(XsAnyType, _, _) =>
      ext.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil }
    case _ => Nil
  }
  
  def splitSequences(namespace: Option[String], family: List[String],
        compositor: HasParticle): List[SequenceDecl] = compositor match {
    case seq: SequenceDecl if seq.particles.size > contentsSizeLimit || isWrapped(namespace, family) =>
       splitLong[SequenceDecl](seq.particles) { xs => SequenceDecl(namespace, xs, 1, 1, 0) }
    case _ => Nil
  }
     
  def flattenElements(namespace: Option[String], family: List[String],
      compositor: HasParticle, index: Int, wrapTopSequence: Boolean): List[ElemDecl] = {    
    compositor match {
      case ref:GroupRef => flattenElements(namespace, family, buildGroup(ref), index, wrapTopSequence)

      case group:GroupDecl =>
        if (primaryCompositor(group).particles.isEmpty) Nil
        else List(buildCompositorRef(group, index))

      case seq: SequenceDecl =>
        if (wrapTopSequence &&
          (seq.minOccurs != 1 || seq.maxOccurs != 1))
          if (seq.particles.size == 1) compositor.particles(0) match {
            case any: AnyDecl => List(buildAnyRef(any.copy(
              minOccurs = math.min(any.minOccurs, seq.minOccurs),
              maxOccurs = math.max(any.maxOccurs, seq.maxOccurs)) ))
            case choice: ChoiceDecl => 
              val occurence = mergeOccurrence(buildOccurrence(choice).copy(nillable = false),
                buildOccurrence(seq))
              List(buildCompositorRef(choice, occurence, 0))
            case group: GroupDecl => flattenElements(namespace, family, group, index, wrapTopSequence)
            case _ => List(buildCompositorRef(seq, index))
          }
          else List(buildCompositorRef(seq, index))
        else splitLongSequence(
            namespace, family, compositor.particles).zipWithIndex flatMap {
          case (ref: GroupRef, i: Int)            => flattenElements(namespace, family, buildGroup(ref), i + index, wrapTopSequence)
          case (compositor2: HasParticle, i: Int) => List(buildCompositorRef(compositor2, i + index))
          case (elem: ElemDecl, i: Int)           => List(elem)
          case (ref: ElemRef, i: Int)             => List(buildElement(ref))
          case (any: AnyDecl, i: Int)             => List(buildAnyRef(any))
        }
      case all: AllDecl =>
        if (isLongAll(all, namespace, family)) List(buildLongAllRef(all))
        else compositor.particles flatMap {
           // by spec, <all> contains only elems.
          case elem: ElemDecl           => List(toOptional(elem))
          case ref: ElemRef             => List(toOptional(buildElement(ref)))  
        }

      case choice: ChoiceDecl =>
        List(buildCompositorRef(choice, index))
    }          
  }
  
  def isLongAll(all: AllDecl, namespace: Option[String], family: List[String]): Boolean =
    (all.particles.size > contentsSizeLimit || isWrapped(namespace, family))
  
  val buildSimpleTypeRef: ComplexTypeContent =>? List[ElemDecl] = {
    case content: ComplexTypeContent
        if content.base.isInstanceOf[BuiltInSimpleTypeSymbol] =>
      val symbol = content.base.asInstanceOf[BuiltInSimpleTypeSymbol]
      List(buildSymbolElement(symbol))
    case content: ComplexTypeContent
        if content.base.isInstanceOf[ReferenceTypeSymbol] &&
        content.base.asInstanceOf[ReferenceTypeSymbol].decl.isInstanceOf[SimpleTypeDecl] =>
      val symbol = content.base.asInstanceOf[ReferenceTypeSymbol]
      List(buildSymbolElement(symbol))
  } 
  
  def generateAccessors(all: AllDecl): List[String] = {
    val wrapperName = makeParamName("all", false)
    
    // by spec, there are only elements under <all>
    all.particles collect {
      case elem: ElemDecl => elem
      case ref: ElemRef   => buildElement(ref)
    } map { elem => toCardinality(elem.minOccurs, elem.maxOccurs) match {
        case Optional => "lazy val " + makeParamName(elem.name, false) + " = " +
          wrapperName + ".get(" +  quote(buildNodeName(elem, true)) + ") map { _.as[" + buildTypeName(elem.typeSymbol) + "] }"
        case _        => "lazy val " + makeParamName(elem.name, false) + " = " +
          wrapperName + "(" +  quote(buildNodeName(elem, true)) + ").as[" + buildTypeName(elem.typeSymbol) + "]"
      }
    }
  }
  
  def generateAccessors(attributes: List[AttributeLike]): List[String] = {
    val wrapperName = makeParamName(ATTRS_PARAM, false)

    attributes collect {
      case attr: AttributeDecl   => (attr, toCardinality(attr))
      case ref: AttributeRef     =>
        val attr = buildAttribute(ref)
        (attr, toCardinality(attr))
      case group: AttributeGroupDecl => (group, Single)
    } collect {
      case (attr: AttributeDecl, Optional) =>
        "lazy val " + makeParamName(buildParam(attr).name, true) + " = " +
          wrapperName + ".get(" +  quote(buildNodeName(attr, false)) + ") map { _.as[" + buildTypeName(attr.typeSymbol, true) + "] }"
      case (attr: AttributeDecl, Single) =>
        "lazy val " + makeParamName(buildParam(attr).name, true) + " = " +
          wrapperName + "(" +  quote(buildNodeName(attr, false)) + ").as[" + buildTypeName(attr.typeSymbol, true) + "]"
    }
  }
  
  def generateAccessors(params: List[Param], splits: List[SequenceDecl]) = params flatMap {
    case param@Param(_, _, ReferenceTypeSymbol(decl@ComplexTypeDecl(_, _, _, _, _, _, _, _)), _, _, _, _, _) if
        compositorWrapper.contains(decl) &&
        splits.contains(compositorWrapper(decl)) =>  
      val wrapperName = makeParamName(param.name, false)
      val particles = compositorWrapper(decl).particles.zipWithIndex flatMap {
        case (ref: GroupRef, i: Int)            => List(buildCompositorRef(ref, i))
        case (compositor2: HasParticle, i: Int) => List(buildCompositorRef(compositor2, i))
        case (elem: ElemDecl, i: Int)           => List(elem)
        case (ref: ElemRef, i: Int)             => List(buildElement(ref))
        case (any: AnyDecl, i: Int)             => List(buildAnyRef(any))
      }
      
      val paramList = particles map { buildParam }
      paramList map { p =>
        "lazy val " + makeParamName(p.name, false) + " = " + wrapperName + "." +  makeParamName(p.name, false)
      }
    case _ => Nil
  }

  def buildParticles(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    anyNumbers.clear()
    
    val build: ComplexTypeContent =>? List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) =>
        buildParticles(base, makeTypeName(base.name))
      
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        buildParticles(base, makeTypeName(base.name))
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        buildParticles(base, makeTypeName(base.name))        
      case res@CompContRestrictionDecl(XsAnyType, _, _) =>
        buildParticles(res.compositor, name)
      
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        buildParticles(base, makeTypeName(base.name)) :::
          buildParticles(ext.compositor, name)
      case ext@CompContExtensionDecl(XsAnyType, _, _) =>
        buildParticles(ext.compositor, name)
        
      case _ => Nil
    }
    
    val pf = buildSimpleTypeRef orElse  build
    
    pf(decl.content.content)
  }

  def flattenMixed(decl: ComplexTypeDecl) = if (buildEffectiveMixed(decl))
    List(ElemDecl(Some(INTERNAL_NAMESPACE), MIXED_PARAM, XsMixed,
      None, None, 0, Integer.MAX_VALUE))
  else Nil
    
//  def buildAttributes(decl: ComplexTypeDecl): List[AttributeLike] = {
//    val attributes = mergeAttributes(decl.content.content match {
//      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) => buildAttributes(base)
//      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) => buildAttributes(base)
//      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) => buildAttributes(base)
//      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) => buildAttributes(base)
//      case _ => Nil
//    }, buildAttributes(decl.content.content.attributes))
//
//    // rearrange attributes so AnyAttributeDecl comes at the end.
//    val notAnyAttributes = attributes filter {
//      case any: AnyAttributeDecl => false
//      case _ => true
//    }
//    val anyAttributes = attributes filter {
//      case any: AnyAttributeDecl => true
//      case _ => false
//    }
//    if (anyAttributes.isEmpty) notAnyAttributes
//    else notAnyAttributes ::: List(anyAttributes.head)
//  }
//
//  def buildAttributes(attributes: List[AttributeLike]): List[AttributeLike] =
//    attributes map(resolveRef)
//

  def makeSchemaComment = <source>{makeAnnotation(schema.annotation)}</source>
  
  def makeAnnotation(anno: Option[AnnotationDecl]) = anno match {
    case Some(annotation) =>
      newline + "/** " +
      (for (doc <- annotation.documentations;
        x <- doc.any)
          yield x.toString).mkString + newline +
      "*/"
    case None => ""    
  }
}
