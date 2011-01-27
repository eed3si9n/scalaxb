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

import scalaxb.compiler.{Logger, Config, Snippet}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._

abstract class GenSource(val schema: SchemaDecl,
    val context: XsdContext) extends Parsers with XMLOutput {  
  type =>?[A, B] = PartialFunction[A, B]
  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val choices = schema.choices
  val MIXED_PARAM = "mixed"
  
  def run: Snippet = {
    log("xsd: GenSource.run")
    
    val nodes = mutable.ListBuffer.empty[Node]
    val companions = mutable.ListBuffer.empty[Node]
    val implicitValues = mutable.ListBuffer.empty[Node]
    def splitSnippet(snippet: Snippet) {
      nodes ++= snippet.definition
      companions ++= snippet.companion
      implicitValues ++= snippet.implicitValue
    }
    
    nodes += makeSchemaComment
    nodes += makePackageName(packageName(schema, context))
    
    schema.typeList map {
      case decl: ComplexTypeDecl =>
        if (context.baseToSubs.keysIterator.contains(decl)) {
          splitSnippet(makeTrait(decl))
          if (!decl.abstractValue) splitSnippet(makeSuperType(decl))
        }
        else splitSnippet(makeType(decl))
        
      case decl: SimpleTypeDecl =>
        if (containsEnumeration(decl)) splitSnippet(makeEnumType(decl))
    }
    
    for ((sch, group) <- context.groups if sch == this.schema)
      splitSnippet(makeGroup(group))  
    for (group <- schema.topAttrGroups.valuesIterator)
      splitSnippet(makeAttributeGroup(group))
    
    Snippet(nodes, companions, implicitValues)
  }
    
  def makeSuperType(decl: ComplexTypeDecl): Snippet = {
    val localName = makeProtectedTypeName(schema.targetNamespace, decl, context)
    val fqn = buildFullyQualifiedName(schema, localName)
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
        case Nil     => error("Type not found: {" + namespace + "}:" + name)
      }
      
  def makeTrait(decl: ComplexTypeDecl): Snippet = {
    val localName = buildTypeName(decl, true)
    val fqn = buildTypeName(decl, false)
    val formatterName = buildFormatterName(decl.namespace, localName)
    log("GenSource.makeTrait: emitting " + fqn)

    val childElements = if (decl.mixed) Nil
      else flattenElements(decl, 0)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list map { buildParam }
    val defaultType = buildFullyQualifiedName(schema, makeProtectedTypeName(schema.targetNamespace, decl, context))    
    val argList = list map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl, false)
      case x => buildArg(x)
    }
    val superNames = buildSuperNames(decl)
    
    val extendString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) =
      "case (" + quoteNamespace(decl.namespace) + ", " + quote(Some(decl.family)) + ") => " + 
        "Right(" + buildFromXML(buildTypeName(decl, false), "node") + ")"
    
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
    val companionCode = <source>  override def build{formatterName} = new Default{formatterName} {{}}
  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    { // if (imports.isEmpty) ""
      //  else imports.mkString(newline + indent(2)) + newline + indent(2) 
    }def reads(seq: scala.xml.NodeSeq): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node =>     
        scalaxb.Helper.instanceType(node) match {{
          { val cases = for (sub <- context.baseToSubs(decl))
              yield makeCaseEntry(sub)
            cases.mkString(newline + indent(4 + compDepth))        
          }
          { if (!decl.abstractValue) "case _ => Right(" + buildFromXML(defaultType, "node") + ")"
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
        else """case _ => error("Unknown type: " + __obj)"""
      }
    }}
  }}</source>
    
    val compositorCodes = if (decl.abstractValue) compositors map { makeCompositor }
      else Nil
    
    Snippet(Seq(traitCode) ++ compositorCodes.flatMap(_.definition),
     Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
     Seq(makeImplicitValue(fqn, formatterName)) ++ compositorCodes.flatMap(_.implicitValue))
  }
  
  def makeImplicitValue(fqn: String, formatterName: String): Node =
    <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{fqn}] = build{formatterName}
  def build{formatterName}: scalaxb.XMLFormat[{fqn}]</source>
  
  def makeImplicitValue(group: AttributeGroupDecl): Node = {
    val formatterName = buildFormatterName(group)
    val fqn = buildTypeName(group, false)
    <source>  implicit lazy val {formatterName}: scalaxb.AttributeGroupFormat[{fqn}] = build{formatterName}
  def build{formatterName}: scalaxb.AttributeGroupFormat[{fqn}]</source>
  }
  
  def makeCaseClassWithType(localName: String, fqn: String, decl: ComplexTypeDecl): Snippet = {
    log("GenSource#makeCaseClassWithType: emitting " + fqn)
    val formatterName = buildFormatterName(decl.namespace, localName)
    
    val primary = decl.content match {
      case ComplexContentDecl(CompContRestrictionDecl(_, x, _)) => x
      case ComplexContentDecl(CompContExtensionDecl(_, x, _)) => x
      case _ => None
    }
        
    val superNames: List[String] = if (context.baseToSubs.contains(decl))
      List(buildTypeName(decl, true))
    else buildSuperNames(decl)
    
    val flatParticles = flattenElements(decl, 0)
    // val particles = buildParticles(decl, name)
    val childElements = if (decl.mixed) flattenMixed(decl)
      else flatParticles 
    val attributes = flattenAttributes(decl)
    val longAttribute = (attributes.size + childElements.size > MaxParticleSize &&
      childElements.size + 1 <= MaxParticleSize)
    val list = if (longAttribute) List.concat[Decl](childElements, List(buildLongAttributeRef))
      else List.concat[Decl](childElements, attributes)
    if (list.size > 22) error("A case class with > 22 parameters cannot be created: " +
      fqn + ": " + decl)
    
    val paramList = list map { buildParam }
    // val dependents = ((flatParticles flatMap { buildDependentType } collect {
    //   case ReferenceTypeSymbol(d: ComplexTypeDecl) if d != decl => d
    // }).toList ++ (attributes collect {
    //   case group: AttributeGroupDecl => group
    // }).toList).distinct
    
    val unmixedParserList = flatParticles map { buildParser(_, decl.mixed, decl.mixed) }
    val parserList = if (decl.mixed) buildTextParser :: (unmixedParserList flatMap { List(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList = ( 0 to parserList.size - 1) map { buildSelector }
    
    val longAll: Boolean = primary match {
        case Some(all: AllDecl) if isLongAll(all, decl.namespace, decl.family) => true
        case _ => false
      }
    val particleArgs = if (decl.mixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(flatParticles((i - 1) / 2), i)
        else buildArgForOptTextRecord(i) }
      else primary match {
        case Some(all: AllDecl) => all.particles map { buildArgForAll(_, longAll) }
        case _ => (0 to flatParticles.size - 1).toList map { i => buildArg(flatParticles(i), i) }
      }
    
    val accessors = (primary match {
        case Some(all: AllDecl) if longAll => generateAccessors(all)
        case _ => generateAccessors(paramList, splitSequences(decl))
      }) ::: (if (longAttribute) generateAccessors(attributes) else Nil)
    log("GenSource#makeCaseClassWithType: generateAccessors " + accessors)
        
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator.toList
        
    val extendString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) && (!decl.mixed) && (!longAll)
    
    def paramsString = if (hasSequenceParam) makeParamName(paramList.head.name) + ": " + 
        paramList.head.singleTypeName + "*"
      else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))
    
    val simpleFromXml: Boolean = if (flatParticles.isEmpty && !decl.mixed) true
      else (decl.content, primary) match {
        case (x: SimpleContentDecl, _) => true
        case (_, Some(all: AllDecl)) => true
        case _ => false
      }
    
    def argsString = if (hasSequenceParam) particleArgs.head + ": _*"
      else {
        val particleString = if (decl.mixed) "Seq.concat(" + particleArgs.mkString("," + newline + indent(4)) + ")"
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
                    buildArgForAttribute(x, longAttribute) }).mkString("," + newline + indent(4)) + newline +
                  indent(4)  + ").flatten[(String, scalaxb.DataRecord[Any])]"
              val anyString = if (anyAttributes.isEmpty) ""
                else "(" + buildArgForAnyAttribute(decl, longAttribute) + ")"
              "scala.collection.immutable.ListMap(" + 
                (if (nonAnyString != "" && anyString != "") nonAnyString + " ::: " + anyString
                 else nonAnyString + anyString ) + ": _*)"
            } 
            else (attributes map {
                    case any: AnyAttributeDecl => buildArgForAnyAttribute(decl, longAttribute)
                    case x => buildArgForAttribute(x, longAttribute) 
                 }).mkString("," + newline + indent(4))              
          } // if-else
          
        if (!particleString.isEmpty && !attributeString.isEmpty) particleString + "," + newline +
          indent(4) + attributeString
        else particleString + attributeString
      }
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def makeWritesChildNodes = <source>    def writesChildNodes(__obj: {fqn}, __scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node] =
      {childString}</source>
    
    def childString = if (decl.mixed) "__obj." + makeParamName(MIXED_PARAM) + 
      ".toSeq flatMap { x => toXML[scalaxb.DataRecord[Any]](x, x.namespace, x.key, __scope, false) }"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _, _, _) => "Seq(scala.xml.Text(__obj.value.toString))"
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>   "Seq(scala.xml.Text(__obj.value.toString))"
      case _ =>
        if (childElemParams.isEmpty) "Nil"
        else if (childElemParams.size == 1) "(" + buildXMLString(childElemParams(0)) + ")"
        else childElemParams.map(x => 
          buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
    }
    
    val groups = filterGroup(decl)
    val companionSuperNames: List[String] = "scalaxb.ElemNameParser[" + fqn + "]" :: groups.map(g => 
      buildFormatterName(g.namespace, groupTypeName(g)))
    
    val caseClassCode = <source>{ buildComment(decl) }case class {localName}({paramsString}){extendString}{ if (accessors.size == 0) ""
      else " {" + newline +
        indent(1) + accessors.mkString(newline + indent(1)) + newline +
        "}" + newline }</source>
    
    def companionCode = if (simpleFromXml) <source>  override def build{formatterName} = new Default{formatterName} {{}}
  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] with scalaxb.CanWriteChildNodes[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def reads(seq: scala.xml.NodeSeq): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node => Right({fqn}({argsString}))
      case _ => Left("reads failed: seq must be scala.xml.Node")
    }}
    
{makeWritesAttribute}{makeWritesChildNodes}
  }}</source>
    else <source>  override def build{formatterName} = new Default{formatterName} {{}}
  trait Default{formatterName} extends {companionSuperNames.mkString(" with ")} {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    { if (decl.isNamed) "override def typeName: Option[String] = Some(" + quote(decl.name) + ")" + newline + newline + indent(2)  
      else ""
    }{ if (decl.mixed) "override def isMixed: Boolean = true" + newline + newline + indent(2)
       else "" }def parser(node: scala.xml.Node): Parser[{fqn}] =
      { parserList.mkString(" ~ " + newline + indent(3)) } ^^
      {{ case { parserVariableList.mkString(" ~ ") } =>
      {fqn}({argsString}) }}
    
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
      __obj.{makeParamName(ATTRS_PARAM)}.toList map {{
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
    
    Snippet(Seq(caseClassCode) ++ compositorCodes.flatMap(_.definition),
      Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
      Seq(makeImplicitValue(fqn, formatterName)) ++ compositorCodes.flatMap(_.implicitValue))
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
    val fqn = buildFullyQualifiedName(schema, localName)
    val formatterName = buildFormatterName(schema.targetNamespace, localName)
    
    // pass in local name for the family.
    // since the sequence is already split at this point, it does not require resplitting.
    val particles = flattenElements(schema.targetNamespace, localName, seq, 0, false)
    val paramList = particles map { buildParam }
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute)
    val paramsString = if (hasSequenceParam)
        makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
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
     <source>  override def build{formatterName} = new Default{formatterName} {{}} 
  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def reads(seq: scala.xml.NodeSeq): Either[String, {fqn}] = Left("don't call me.")
    
{makeWritesXML}
  }}</source>,
      makeImplicitValue(fqn, formatterName))
  }
    
  def makeGroup(group: GroupDecl): Snippet = {
    val compositors = context.compositorParents.filter(
      x => x._2 == makeGroupComplexType(group)).keysIterator.toList
    val localName = makeTypeName(context.compositorNames(group))
    val fqn = buildFullyQualifiedName(schema, localName)
    val formatterName = buildFormatterName(group.namespace, localName)
    
    val compositor = primaryCompositor(group)
    val param = buildParam(compositor)
    val wrapperParam = compositor match {
      case choice: ChoiceDecl => param
      case _ => param.copy(typeSymbol = XsDataRecord(param.typeSymbol))
    }
    val mixedParam = param.copy(typeSymbol = XsDataRecord(XsAnyType))
    val parser = buildCompositorParser(compositor, SingleUnnillable, false, false)
    val wrapperParser = compositor match {
      case choice: ChoiceDecl => parser
      case _ => buildCompositorParser(compositor, SingleUnnillable, false, true)
    }
    val mixedparser = buildCompositorParser(compositor, SingleUnnillable, true, true)
    
    val groups = filterGroup(compositor)
    val superNames: List[String] = 
      if (groups.isEmpty) List("scalaxb.AnyElemNameParser")
      else groups.map { g => buildFormatterName(g.namespace, groupTypeName(g)) }
    
    val companionCode = <source>{ buildComment(group) }  trait {formatterName} extends {superNames.mkString(" with ")} {{  
    private val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def parse{localName}: Parser[{param.baseTypeName}] =
      {parser}
  
    def parse{localName}(wrap: Boolean): Parser[{wrapperParam.baseTypeName}] =
      {wrapperParser}
    
    def parsemixed{localName}: Parser[Seq[{mixedParam.baseTypeName}]] =
      {mixedparser}
  }}</source>
    
    val compositorCodes = compositors map { makeCompositor }
    Snippet(compositorCodes.flatMap(_.definition),
      Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
      compositorCodes.flatMap(_.implicitValue))
  }
  
  def makeAttributeGroup(group: AttributeGroupDecl): Snippet = {
    val localName = buildTypeName(group, true)
    val fqn = buildTypeName(group, false)
    val formatterName = buildFormatterName(group.namespace, localName)
        
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
    val companionCode = <source>  override def build{formatterName} = new Default{formatterName} {{}} 
  trait Default{formatterName} extends scalaxb.AttributeGroupFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def reads(seq: scala.xml.NodeSeq): Either[String, {fqn}] = seq match {{
      case node: scala.xml.Node => Right({fqn}({argsString}))
      case _ => Left("reads failed: seq must be scala.xml.Node")
    }}
    
    def toAttribute(__obj: {fqn}, __attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = {{
      var attr: scala.xml.MetaData  = __attr
      {attributeString}
      attr
    }}
  }}</source>
    
    Snippet(Seq(caseClassCode),
      Seq(companionCode),
      Seq(makeImplicitValue(group)))
  }
  
  def makeEnumType(decl: SimpleTypeDecl) = {
    val localName = buildTypeName(decl, true)
    val fqn = buildTypeName(decl, false)
    val formatterName = buildFormatterName(decl.namespace, localName)
    val enums = filterEnumeration(decl)
    
    def makeEnum(enum: EnumerationDecl) =
      "case object " + buildTypeName(localName, enum, true) + " extends " + localName + 
      " { override def toString = " + quote(enum.value) + " }"
    
    def makeCaseEntry(enum: EnumerationDecl) =
      indent(2) + "case " + quote(enum.value) + " => " + buildTypeName(localName, enum, true) + newline
    
    val enumString = enums.map(makeEnum).mkString(newline)
    
    val traitCode = enums match {
      case Nil =>
<source>case class {localName}()

object {localName} {{
  def fromString(value: String): {localName} = {localName}()
}}</source>    
      case _ =>
<source>trait {localName}

object {localName} {{
  def fromString(value: String): {localName} = value match {{
{ enums.map(e => makeCaseEntry(e)) }
  }}
}}

{ enumString }</source>
    }  // match
        
    Snippet(traitCode,
      <source>  def build{formatterName} = new Default{formatterName} {{}}
  trait Default{formatterName} extends scalaxb.XMLFormat[{fqn}] {{
    val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
    
    def reads(seq: scala.xml.NodeSeq): Either[String, {fqn}] = Right({fqn}.fromString(seq.text))
    
    def writes(__obj: {fqn}, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      scala.xml.Elem(scalaxb.Helper.getPrefix(__namespace, __scope).orNull, 
        __elementLabel getOrElse {{ error("missing element label.") }},
        scala.xml.Null, __scope, scala.xml.Text(__obj.toString))
  }}</source>,
      makeImplicitValue(fqn, formatterName))
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
        case symbol: ReferenceTypeSymbol =>
          if (symbol.decl == decl && !containsForeignType(choice))
            set += makeTypeName(context.compositorNames(choice))
        case _ => 
      }
    }
    
    for (choice <- choices;
        particle <- choice.particles) particle match {
      case elem: ElemDecl => addIfMatch(elem.typeSymbol, choice)
      case ref: ElemRef   => addIfMatch(buildElement(ref).typeSymbol, choice)      
      case _ => // do nothing
    }
        
    set.toList.distinct
  }
  
  // reverse lookup all choices that contains that.
  def buildOptions(that: HasParticle): List[String] = {
    val set = mutable.Set.empty[String]
    
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
    
    choices foreach { addIfContains }        
    set.toList    
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

  def filterGroup(compositor: Option[HasParticle]): List[GroupDecl] =
      compositor match {
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
      }).distinct
  }
  
  def flattenElements(decl: ComplexTypeDecl, index: Int): List[ElemDecl] = {
    anyNumber = 0
    
    val build: ComplexTypeContent =>? List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) =>
        flattenElements(base, index)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base, index)
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, index)        
      case res@CompContRestrictionDecl(XsAnyType, _, _) =>
        res.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil }
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, index) :::
          (ext.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil })
      case ext@CompContExtensionDecl(XsAnyType, _, _) =>
        ext.compositor map { flattenElements(decl.namespace, decl.family, _, index, true) } getOrElse { Nil }
      case _ => Nil
    }
    
    val pf = buildSimpleTypeRef orElse build
    pf(decl.content.content)
  }
  
  // sometimes we don't have ComplexTypeDecl because it's a group.
  def splitLongSequence(namespace: Option[String], family: String, particles: List[Particle]): List[Particle] =
    if (particles.size <= MaxParticleSize && !isWrapped(namespace, family)) particles
    else splitLong[SequenceDecl](particles) { SequenceDecl(_, 1, 1, 0) }
  
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
  
  def splitSequences(namespace: Option[String], family: String,
        compositor: HasParticle): List[SequenceDecl] = compositor match {
    case seq: SequenceDecl if seq.particles.size > MaxParticleSize || isWrapped(namespace, family) =>
       splitLong[SequenceDecl](seq.particles) { xs => SequenceDecl(xs, 1, 1, 0) }
    case _ => Nil
  }
     
  def flattenElements(namespace: Option[String], family: String,
      compositor: HasParticle, index: Int, wrapTopSequence: Boolean): List[ElemDecl] = {    
    compositor match {
      case ref:GroupRef =>
        List(buildCompositorRef(ref, index))

      case group:GroupDecl =>
        List(buildCompositorRef(group, index))

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
            case _ => List(buildCompositorRef(seq, index))
          }
          else List(buildCompositorRef(seq, index))
        else splitLongSequence(
            namespace, family, compositor.particles).zipWithIndex flatMap {
          case (ref: GroupRef, i: Int)            => List(buildCompositorRef(ref, i))
          case (compositor2: HasParticle, i: Int) => List(buildCompositorRef(compositor2, i))
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
  
  def isLongAll(all: AllDecl, namespace: Option[String], family: String): Boolean =
    (all.particles.size > MaxParticleSize || isWrapped(namespace, family))
  
  val buildSimpleTypeRef: ComplexTypeContent =>? List[ElemDecl] = {
    case content: ComplexTypeContent
        if content.base.isInstanceOf[BuiltInSimpleTypeSymbol] =>
      val symbol = content.base.asInstanceOf[BuiltInSimpleTypeSymbol]
      List(buildElement(symbol))
    case content: ComplexTypeContent
        if content.base.isInstanceOf[ReferenceTypeSymbol] &&
        content.base.asInstanceOf[ReferenceTypeSymbol].decl.isInstanceOf[SimpleTypeDecl] =>
      val symbol = content.base.asInstanceOf[ReferenceTypeSymbol].decl.asInstanceOf[SimpleTypeDecl]
      List(buildElement(symbol))    
  } 
  
  def generateAccessors(all: AllDecl): List[String] = {
    val wrapperName = makeParamName("all")
    
    // by spec, there are only elements under <all>
    all.particles collect {
      case elem: ElemDecl => elem
      case ref: ElemRef   => buildElement(ref)
    } map { elem => toCardinality(elem.minOccurs, elem.maxOccurs) match {
        case Optional => "lazy val " + makeParamName(elem.name) + " = " + 
          wrapperName + ".get(" +  quote(buildNodeName(elem, true)) + ") map { _.as[" + buildTypeName(elem.typeSymbol) + "] }"
        case _        => "lazy val " + makeParamName(elem.name) + " = " + 
          wrapperName + "(" +  quote(buildNodeName(elem, true)) + ").as[" + buildTypeName(elem.typeSymbol) + "]"
      }
    }
  }
  
  def generateAccessors(attributes: List[AttributeLike]): List[String] = {
    val wrapperName = makeParamName(ATTRS_PARAM)

    attributes collect {
      case attr: AttributeDecl   => (attr, toCardinality(attr))
      case ref: AttributeRef     =>
        val attr = buildAttribute(ref)
        (attr, toCardinality(attr))
      case group: AttributeGroupDecl => (group, Single)
    } map { _ match {
        case (attr: AttributeDecl, Optional) => "lazy val " + makeParamName(buildParam(attr).name) + " = " +
          wrapperName + ".get(" +  quote(buildNodeName(attr, false)) + ") map { _.as[" + buildTypeName(attr.typeSymbol, true) + "] }"
        case (attr: AttributeDecl, Single) => "lazy val " + makeParamName(buildParam(attr).name) + " = " +
          wrapperName + "(" +  quote(buildNodeName(attr, false)) + ").as[" + buildTypeName(attr.typeSymbol, true) + "]"

      }
    }
  }
  
  def generateAccessors(params: List[Param], splits: List[SequenceDecl]) = params flatMap {
    case param@Param(_, _, ReferenceTypeSymbol(decl@ComplexTypeDecl(_, _, _, _, _, _, _, _)), _, _, _, _, _) if
        compositorWrapper.contains(decl) &&
        splits.contains(compositorWrapper(decl)) =>  
      val wrapperName = makeParamName(param.name)
      val particles = compositorWrapper(decl).particles.zipWithIndex flatMap {
        case (ref: GroupRef, i: Int)            => List(buildCompositorRef(ref, i))
        case (compositor2: HasParticle, i: Int) => List(buildCompositorRef(compositor2, i))
        case (elem: ElemDecl, i: Int)           => List(elem)
        case (ref: ElemRef, i: Int)             => List(buildElement(ref))
        case (any: AnyDecl, i: Int)             => List(buildAnyRef(any))
      }
      
      val paramList = particles map { buildParam }
      paramList map { p =>
        "lazy val " + makeParamName(p.name) + " = " + wrapperName + "." +  makeParamName(p.name)
      }
    case _ => Nil
  }
  
  def buildParticles(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    anyNumber = 0
    
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
      
  def buildElement(decl: SimpleTypeDecl): ElemDecl = decl.content match {
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _) => buildElement(base)
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol, _) => buildElement(base)
    case _ => error("GenSource: unsupported type: " + decl)
  }
          
  def flattenMixed(decl: ComplexTypeDecl) = if (decl.mixed)
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

  def makeSchemaComment = 
    <source>// Generated by &lt;a href="http://scalaxb.org/"&gt;scalaxb&lt;/a&gt;.{makeAnnotation(schema.annotation)}</source>
  
  def makeAnnotation(anno: Option[AnnotationDecl]) = anno match {
    case Some(annotation) =>
      newline + "/** " +
      (for (doc <- annotation.documentations;
        x <- doc.any)
          yield x.toString).mkString + newline +
      "*/"
    case None => ""    
  }
  
  def makePackageName(pkg: Option[String]) = pkg map { x => 
    <source>package {x}</source> } getOrElse { <source></source> }
}
