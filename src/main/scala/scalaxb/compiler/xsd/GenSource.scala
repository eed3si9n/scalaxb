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

import scalaxb.compiler.{Logger, Config}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._

abstract class GenSource(val schema: SchemaDecl,
    val dependentSchemas: Seq[SchemaDecl],
    val context: XsdContext) extends Parsers with XMLOutput {  
  type =>?[A, B] = PartialFunction[A, B]
  
  case class Snippet(defition: Seq[Node],
    companion: Seq[Node] = <source/>,
    implicitValue: Seq[Node]  = <source/>)
  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val choices = schema.choices
  val MIXED_PARAM = "mixed"
  
  def run: Seq[Node] = {
    log("xsd: GenSource.run")
    
    val nodes = mutable.ListBuffer.empty[Node]
    val companions = mutable.ListBuffer.empty[Node]
    val implicitValues = mutable.ListBuffer.empty[Node]
    def splitSnippet(snippet: Snippet) {
      nodes ++= snippet.defition
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
    
    nodes += makeXMLProtocol(companions, implicitValues)
    nodes
  }
  
  def makeXMLProtocol(companions: Seq[Node], implicitValues: Seq[Node]) = {
    val typeNames = context.typeNames(packageName(schema, context))
    val name = typeNames(schema)
    val imports = dependentSchemas map { sch =>
      val pkg = packageName(sch, context)
      val name = context.typeNames(pkg)(sch)
      "import " + pkg.map(_ + ".").getOrElse("") + name + "._"
    }
    
    <source>object {name} {{
  import scalaxb.Scalaxb._
  import scalaxb.XMLFormat._
  { if (imports.isEmpty) ""
    else imports.mkString(newline + indent(1)) + newline }
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
{implicitValues}

{companions}
}}</source>
  }
  
  def makeSuperType(decl: ComplexTypeDecl): Snippet =
    makeCaseClassWithType(makeProtectedTypeName(schema.targetNamespace, decl, context), decl)
      
  def makeType(decl: ComplexTypeDecl): Snippet = {
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    makeCaseClassWithType(typeNames(decl), decl)
  }
  
  def types(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topTypes.contains(name))
      yield schema.topTypes(name)) match {
        case x :: xs => x
        case Nil     => error("Type not found: {" + namespace + "}:" + name)
      }
      
  def makeTrait(decl: ComplexTypeDecl): Snippet = {
    val name = buildTypeName(decl)
    val formatterName = name + "Format"
    log("GenSource.makeTrait: emitting " + name)

    val childElements = if (decl.mixed) Nil
      else flattenElements(decl)
    val list = List.concat[Decl](childElements, buildAttributes(decl))
    val paramList = list.map { buildParam }
    val defaultType = makeProtectedTypeName(schema.targetNamespace, decl, context)    
    val argList = list map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
      case x => buildArg(x)
    }
    val superNames = buildSuperNames(decl)
    
    val extendString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) =
      "case (" + quoteNamespace(decl.namespace) + ", " + quote(decl.family) + ") => " + 
        "Right(" + buildFromXML(buildTypeName(decl), "node") + ")"
    
    def makeToXmlCaseEntry(decl: ComplexTypeDecl) =
      "case x: " + buildTypeName(decl) + " => " + 
        buildToXML(buildTypeName(decl), "x, __namespace, __elementLabel, __scope, true")
        
    val compositors = context.compositorParents.filter(_._2 == decl).keysIterator.toList
    val extendedSubtypes = context.baseToSubs(decl) filter { sub =>
      !schema.typeList.contains(sub) && !dependentSchemas.exists(_.typeList.contains(sub)) }
    val extendedSchemas = (for (sub <- extendedSubtypes;
                                sch <- context.schemas; if sch.typeList.contains(sub))
                             yield sch).toList.distinct
    val imports = extendedSchemas map { sch =>
      val pkg = packageName(sch, context)
      val name = context.typeNames(pkg)(sch)
      "import " + pkg.map(_ + ".").getOrElse("") + name + "._"
    }  
    
    val traitCode = <source>{ buildComment(decl) }trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param.toScalaCode
  vals.mkString(newline + indent(1))}
}}</source>
    
    val compDepth = 1
    val implicitValueCode = <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{name}] = __{formatterName}</source>
    val companionCode = <source>  val __{formatterName} = new scalaxb.XMLFormat[{name}] {{
    { if (imports.isEmpty) ""
        else imports.mkString(newline + indent(2)) + newline + indent(2) 
    }def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, {name}] = seq match {{
      case node: scala.xml.Node =>     
        scalaxb.Helper.instanceType(node) match {{
          { val cases = for (sub <- context.baseToSubs(decl))
              yield makeCaseEntry(sub)
            cases.mkString(newline + indent(4 + compDepth))        
          }
          { if (!decl.abstractValue) "case _ => Right(" + buildFromXML(defaultType, "node") + ")"
            else """case x => Left("Unknown type: " + x)""" }
        }}
      case _ => Left("readsXMLEither failed: seq must be scala.xml.Node")  
    }}
    
    def writesXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String],
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
    
    Snippet(Seq(traitCode) ++ compositorCodes.flatMap(_.defition),
     Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
     Seq(implicitValueCode) ++ compositorCodes.flatMap(_.implicitValue))
  }
        
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): Snippet = {
    log("GenSource#makeCaseClassWithType: emitting " + name)
    
    val formatterName = name + "Format"
    
    val primary = decl.content match {
      case ComplexContentDecl(CompContRestrictionDecl(_, x, _)) => x
      case ComplexContentDecl(CompContExtensionDecl(_, x, _)) => x
      case _ => None
    }
    
    val superNames: List[String] = if (context.baseToSubs.contains(decl))
      List(buildTypeName(decl))
    else buildSuperNames(decl)
    
    val flatParticles = flattenElements(decl)
    // val particles = buildParticles(decl, name)
    val childElements = if (decl.mixed) flattenMixed(decl)
      else flatParticles 
    val attributes = buildAttributes(decl)    
    val list = List.concat[Decl](childElements, attributes)
    
    if (list.size > 22) error("A case class with > 22 parameters cannot be created.")
    
    val paramList = list.map { buildParam }
    // val dependents = ((flatParticles flatMap { buildDependentType } collect {
    //   case ReferenceTypeSymbol(d: ComplexTypeDecl) if d != decl => d
    // }).toList ++ (attributes collect {
    //   case group: AttributeGroupDecl => group
    // }).toList).distinct
    
    val unmixedParserList = flatParticles map { buildParser(_, decl.mixed, decl.mixed) }
    val parserList = if (decl.mixed) buildTextParser :: (unmixedParserList flatMap { List(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList = ( 0 to parserList.size - 1) map { buildSelector }
    val accessors = generateAccessors(paramList, splitSequences(decl))
    log("GenSource#makeCaseClassWithType: generateAccessors " + accessors)
    
    val particleArgs = if (decl.mixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(flatParticles((i - 1) / 2), i)
        else buildArgForOptTextRecord(i) }
      else primary match {
        case Some(all: AllDecl) => flatParticles map { p => buildArgForAll(p) }
        case _ => (0 to flatParticles.size - 1).toList map { i => buildArg(flatParticles(i), i) }
      }
    
    var attributeArgs = attributes map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
      case x => buildArg(x) 
    }
    
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator.toList
        
    val extendString = if (superNames.isEmpty) ""
    else " extends " + superNames.mkString(" with ")
    
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!decl.mixed)
    
    def paramsString = if (hasSequenceParam)
      makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
    else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))
    
    val simpleFromXml: Boolean = if (flatParticles.isEmpty && !decl.mixed) true
    else (decl.content, primary) match {
      case (x: SimpleContentDecl, _) => true
      case (_, Some(all: AllDecl)) => true
      case _ => false
    }
    
    def argsString = if (decl.mixed)
      "Seq.concat(" + particleArgs.mkString("," + newline + indent(4)) + ")" +
        (if (attributeArgs.isEmpty) ""
        else "," + newline + indent(4) + attributeArgs.mkString("," + newline + indent(4)))
    else if (hasSequenceParam)
      particleArgs.head + ": _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: attributeArgs).
          mkString("," + newline + indent(4))
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: attributeArgs).
          mkString("," + newline + indent(4))
      case _ =>
        (particleArgs ::: attributeArgs).mkString("," + newline + indent(4))
    }
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def makeWritesChildNodes = <source>    def writesChildNodes(__obj: {name}, __scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node] =
      {childString}</source>
    
    def childString = if (decl.mixed) "__obj." + makeParamName(MIXED_PARAM) + 
      ".flatMap(x => scalaxb.DataRecord.toXML(x, x.namespace, x.key, __scope, false).toSeq)"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _, _) => "Seq(scala.xml.Text(__obj.value.toString))"
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>   "Seq(scala.xml.Text(__obj.value.toString))"
      case _ =>  
        if (childElemParams.isEmpty) "Nil"
        else if (childElemParams.size == 1) "(" + buildXMLString(childElemParams(0)) + ")"
        else childElemParams.map(x => 
          buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
    }
    
    val groups = filterGroup(decl)
    val companionSuperNames: List[String] = "scalaxb.ElemNameParser[" + name + "]" :: groups.map(groupTypeName(_) + "Format")
    
    val caseClassCode = <source>{ buildComment(decl) }case class {name}({paramsString}){extendString}{ if (accessors.size == 0) ""
      else " {" + newline +
        indent(1) + accessors.mkString(newline + indent(1)) + newline +
        "}" + newline }</source>
    
    val implicitValueCode = <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{name}] = __{formatterName}</source>
    def companionCode = if (simpleFromXml) <source>  val __{formatterName} = new scalaxb.XMLFormat[{name}] with scalaxb.CanWriteChildNodes[{name}] {{
    def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, {name}] = seq match {{
      case node: scala.xml.Node => Right({name}({argsString}))
      case _ => Left("readsXMLEither failed: seq must be scala.xml.Node")
    }}
    
{makeWritesAttribute}{makeWritesChildNodes}
  }}</source>
    else <source>  val __{formatterName} = new {companionSuperNames.mkString(" with ")} {{
    { if (decl.isNamed) "override def typeName: Option[String] = Some(" + quote(decl.name) + ")" + newline + newline + indent(2)  
      else ""
    }{ if (decl.mixed) "override def isMixed: Boolean = true" + newline + newline + indent(2)
       else "" }def parser(node: scala.xml.Node): Parser[{name}] =
      { parserList.mkString(" ~ " + newline + indent(3)) } ^^
      {{ case { parserVariableList.mkString(" ~ ") } =>
      {name}({argsString}) }}
    
{makeWritesAttribute}{makeWritesChildNodes}  }}</source>
    
    def makeWritesAttribute = if (attributes.isEmpty) <source></source>
      else <source>    override def writesAttribute(__obj: {name}, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = {{
      var attr: scala.xml.MetaData  = scala.xml.Null
      { attributes.map(x => buildAttributeString(x)).mkString(newline + indent(3)) }
      attr
    }}</source>
    
    val compositorCodes = compositors map { makeCompositor }
    
    Snippet(Seq(caseClassCode) ++ compositorCodes.flatMap(_.defition),
      Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
      Seq(implicitValueCode) ++ compositorCodes.flatMap(_.implicitValue))
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
  
  def makeCompositor(compositor: HasParticle): Snippet = compositor match {
    case seq: SequenceDecl  => makeSequence(seq)
    case _ => 
      val superNames: List[String] = buildOptions(compositor)
      val superString = if (superNames.isEmpty) ""
        else " extends " + superNames.mkString(" with ")
      val name = makeTypeName(context.compositorNames(compositor))
      Snippet(<source>trait {name}{superString}</source>)
  }
  
  def makeCompositorImport(compositor: HasParticle) = compositor match {
    case seq: SequenceDecl =>
      val name = makeTypeName(context.compositorNames(seq))
      <source>import {name}._
  </source> 
    case _ => <source></source>
  }
  
  def makeSequence(seq: SequenceDecl): Snippet = {
    val name = makeTypeName(context.compositorNames(seq))
    val formatterName = name + "Format"
    
    val particles = flattenElements(schema.targetNamespace, name, seq)
    val paramList = particles.map { buildParam }
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute)
    val paramsString = if (hasSequenceParam)
        makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
      else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))    
    def makeWritesXML = <source>    def writesXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String], 
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      {childString}</source>
    def childString = if (paramList.isEmpty) "Nil"
      else if (paramList.size == 1) buildXMLString(paramList(0))
      else paramList.map(x => 
        buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
    val superNames: List[String] = buildOptions(seq)
    val superString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    val implicitValueCode = <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{name}] = __{formatterName}</source>
    
    Snippet(<source>{ buildComment(seq) }case class {name}({paramsString}){superString}</source>,
     <source>  val __{formatterName} = new scalaxb.XMLFormat[{name}] {{
    def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, {name}] = Left("don't call me.")
    
{makeWritesXML}
  }}</source>,
      implicitValueCode)
  }
    
  def makeGroup(group: GroupDecl): Snippet = {
    val compositors = context.compositorParents.filter(
      x => x._2 == makeGroupComplexType(group)).keysIterator.toList
    val name = makeTypeName(context.compositorNames(group))
    val formatterName = name + "Format"
      
    val compositor = primaryCompositor(group)
    val param = buildParam(compositor)
    val wrapperParam = compositor match {
      case choice: ChoiceDecl => param
      case _ => Param(param.namespace, param.name, XsDataRecord(param.typeSymbol),
        param.cardinality, param.nillable, param.attribute)
    }
    val mixedParam = Param(param.namespace, param.name, XsDataRecord(XsAny),
      param.cardinality, param.nillable, param.attribute)
    
    val parser = buildCompositorParser(compositor, SingleUnnillable, false, false)
    val wrapperParser = compositor match {
      case choice: ChoiceDecl => parser
      case _ => buildCompositorParser(compositor, SingleUnnillable, false, true)
    }
    val mixedparser = buildCompositorParser(compositor, SingleUnnillable, true, true)
    
    val groups = filterGroup(compositor)
    val superNames: List[String] = 
      if (groups.isEmpty) List("scalaxb.AnyElemNameParser")
      else groups.map(groupTypeName(_) + "Format")
    
    val companionCode = <source>{ buildComment(group) }  trait {formatterName} extends {superNames.mkString(" with ")} {{  
    def parse{name}: Parser[{param.baseTypeName}] =
      {parser}
  
    def parse{name}(wrap: Boolean): Parser[{wrapperParam.baseTypeName}] =
      {wrapperParser}
    
    def parsemixed{name}: Parser[Seq[{mixedParam.baseTypeName}]] =
      {mixedparser}
  }}</source>
    
    val compositorCodes = compositors map { makeCompositor }
    Snippet(compositorCodes.flatMap(_.defition),
      Seq(companionCode) ++ compositorCodes.flatMap(_.companion),
      compositorCodes.flatMap(_.implicitValue))
  }
  
  def makeAttributeGroup(group: AttributeGroupDecl): Snippet = {
    val name = buildTypeName(group)
    val formatterName = name + "Format"
        
    val attributes = buildAttributes(group.attributes)  
    val paramList = attributes.map { buildParam }
    val argList = attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(group)
        case x => buildArg(x) 
      }
    val paramsString = paramList.map(
      _.toScalaCode).mkString("," + newline + indent(1))
    val argsString = argList.mkString("," + newline + indent(3))  
    val attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    
    Snippet(<source>{ buildComment(group) }case class {name}({paramsString})</source>,
     <source>  object {formatterName} {{
    def fromXML(node: scala.xml.Node): {name} = {{
      {name}({argsString})
    }}
  
    def toAttribute(__obj: {name}, __attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding) = {{
      var attr: scala.xml.MetaData  = __attr
      {attributeString}
      attr
    }} 
  }}</source>)
  }
  
  def makeEnumType(decl: SimpleTypeDecl) = {
    val name = buildTypeName(decl)
    val formatterName = name + "Format"
    val enums = filterEnumeration(decl)
    
    def makeEnum(enum: EnumerationDecl) =
      "case object " + buildTypeName(name, enum) + " extends " + name + 
      " { override def toString = " + quote(enum.value) + " }"
    
    def makeCaseEntry(enum: EnumerationDecl) =
      indent(2) + "case " + quote(enum.value) + " => " + buildTypeName(name, enum) + newline
    
    val enumString = enums.map(makeEnum).mkString(newline)
    
    val traitCode = enums match {
      case Nil =>
<source>case class {name}()

object {name} {{
  def fromString(value: String): {name} = {name}()
}}</source>    
      case _ =>
<source>trait {name}

object {name} {{
  def fromString(value: String): {name} = value match {{
{ enums.map(e => makeCaseEntry(e)) }
  }}
}}

{ enumString }</source>
    }  // match
    
    val implicitValueCode = <source>  implicit lazy val {formatterName}: scalaxb.XMLFormat[{name}] = __{formatterName}</source>
    
    Snippet(traitCode,
      <source>  val __{formatterName} = new scalaxb.XMLFormat[{name}] {{
    def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, {name}] = Right({name}.fromString(seq.text))
    
    def writesXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      scala.xml.Elem(scalaxb.Helper.getPrefix(__namespace, __scope).orNull, 
        __elementLabel getOrElse {{ error("missing element label.") }},
        scala.xml.Null, __scope, scala.xml.Text(__obj.toString))
  }}</source>,
      implicitValueCode)
  }
        
  def flattenSuperNames(decl: ComplexTypeDecl): List[String] = 
    (decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => 
        buildTypeName(base) :: flattenSuperNames(base)
      case _ => Nil
    }) ::: buildOptions(decl)
  
  def buildSuperNames(decl: ComplexTypeDecl) =
    buildSuperName(decl) ::: buildOptions(decl)
  
  def buildSuperName(decl: ComplexTypeDecl) = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => List(buildTypeName(base))
      case _ => Nil
    }
  
  def buildOptions(decl: ComplexTypeDecl): List[String] = {
    val set = mutable.Set.empty[String]
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
        
    set.toList
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
    case res@CompContRestrictionDecl(XsAny, _, _) =>
      filterGroup(res.compositor)
    
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      filterGroup(base) :::
        filterGroup(ext.compositor)
    case ext@CompContExtensionDecl(XsAny, _, _) =>
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
  
  def flattenElements(decl: ComplexTypeDecl): List[ElemDecl] = {
    argNumber = 0
    
    val build: ComplexTypeContent =>? List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base)
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base)        
      case res@CompContRestrictionDecl(XsAny, _, _) =>
        res.compositor map { flattenElements(decl.namespace, decl.family, _) } getOrElse { Nil }
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base) :::
          (ext.compositor map { flattenElements(decl.namespace, decl.family, _) } getOrElse { Nil })
      case ext@CompContExtensionDecl(XsAny, _, _) =>
        ext.compositor map { flattenElements(decl.namespace, decl.family, _) } getOrElse { Nil }
      case _ => Nil
    }
    
    val pf = buildSimpleTypeRef orElse build
    pf(decl.content.content)
  }
  
  def splitLongSequence(namespace: Option[String], family: String,
      particles: List[Particle]): List[Particle] =
    if (particles.size <= MaxParticleSize &&
      !isWrapped(namespace, family)) particles
    else splitLongSequence(particles)
      
  def splitLongSequence(rest: List[Particle]): List[SequenceDecl] =
    if (rest.size <= ChunkParticleSize) List(SequenceDecl(rest, 1, 1))
    else List(SequenceDecl(rest.take(ChunkParticleSize), 1, 1)) :::
      splitLongSequence(rest.drop(ChunkParticleSize))
    
  def flattenElements(namespace: Option[String], family: String,
        compositor: HasParticle): List[ElemDecl] =
      compositor match {
    case ref:GroupRef =>
      List(buildCompositorRef(ref))
      
    case group:GroupDecl =>
      List(buildCompositorRef(group))
    
    case seq: SequenceDecl =>
      splitLongSequence(namespace, family, compositor.particles) flatMap {
        case ref: GroupRef            => List(buildCompositorRef(ref))
        case compositor2: HasParticle => List(buildCompositorRef(compositor2))
        case elem: ElemDecl           => List(elem)
        case ref: ElemRef             => List(buildElement(ref))
        case any: AnyDecl             => List(buildAnyRef(any))
      }
      
    case AllDecl(particles: List[_], _, _) =>
      particles flatMap {
        case ref: GroupRef            => List(buildCompositorRef(ref))
        case compositor2: HasParticle => List(buildCompositorRef(compositor2))
        case elem: ElemDecl           => List(toOptional(elem))
        case ref: ElemRef             => List(buildElement(ref))    
      }
          
    case choice: ChoiceDecl =>
      List(buildCompositorRef(choice))
  }
  
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
  
  def splitSequences(decl: ComplexTypeDecl): List[SequenceDecl] = decl.content.content match {
    case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      splitSequences(base)
    case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
      splitSequences(base)
    
    // complex content means 1. has child elements 2. has attributes
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      splitSequences(base)        
    case res@CompContRestrictionDecl(XsAny, _, _) =>
      res.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil }
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      splitSequences(base) :::
        (ext.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil })
    case ext@CompContExtensionDecl(XsAny, _, _) =>
      ext.compositor map { splitSequences(decl.namespace, decl.family, _) } getOrElse { Nil }
    case _ => Nil    
  }
  
  def splitSequences(namespace: Option[String], family: String,
        compositor: HasParticle): List[SequenceDecl] = compositor match {
    case seq: SequenceDecl if seq.particles.size > MaxParticleSize ||
      isWrapped(namespace, family) => splitLongSequence(seq.particles)
    case _ => Nil
  }
  
  def generateAccessors(params: List[Param], splits: List[SequenceDecl]) = params flatMap {
    case param@Param(_, _, ReferenceTypeSymbol(decl@ComplexTypeDecl(_, _, _, _, _, _, _, _)), _, _, _) if
        compositorWrapper.contains(decl) &&
        splits.contains(compositorWrapper(decl)) =>  
      val wrapperName = makeParamName(param.name)
      val particles = compositorWrapper(decl).particles flatMap {
        case ref: GroupRef            => List(buildCompositorRef(ref))
        case compositor2: HasParticle => List(buildCompositorRef(compositor2))
        case elem: ElemDecl           => List(elem)
        case ref: ElemRef             => List(buildElement(ref))
        case any: AnyDecl             => List(buildAnyRef(any))
      }
            
      val paramList = particles map { buildParam }
      paramList map { p =>
        "def " + makeParamName(p.name) + " = " + wrapperName + "." +  makeParamName(p.name)
      }
    case _ => Nil
  }
  
  def buildParticles(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    argNumber = 0
    
    val build: ComplexTypeContent =>? List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        buildParticles(base, makeTypeName(base.name))
      
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        buildParticles(base, makeTypeName(base.name))
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        buildParticles(base, makeTypeName(base.name))        
      case res@CompContRestrictionDecl(XsAny, _, _) =>
        buildParticles(res.compositor, name)
      
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        buildParticles(base, makeTypeName(base.name)) :::
          buildParticles(ext.compositor, name)
      case ext@CompContExtensionDecl(XsAny, _, _) =>
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
      None, None, 0, Integer.MAX_VALUE, None, None, None))
  else Nil
    
  def buildAttributes(decl: ComplexTypeDecl): List[AttributeLike] =
    mergeAttributes(decl.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) => 
        buildAttributes(base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) =>
        buildAttributes(base)
      case _ => Nil
    }, buildAttributes(decl.content.content.attributes))

  def buildAttributes(attributes: List[AttributeLike]): List[AttributeLike] =
    attributes map(resolveRef)
  
  def resolveRef(attribute: AttributeLike): AttributeLike = attribute match {
    case any: AnyAttributeDecl => any
    case attr: AttributeDecl => attr
    case ref: AttributeRef   => buildAttribute(ref)
    case group: AttributeGroupDecl => group
    case ref: AttributeGroupRef    => buildAttributeGroup(ref)    
  }
    
  def mergeAttributes(parent: List[AttributeLike],
      child: List[AttributeLike]): List[AttributeLike] = child match {
    case x :: xs => mergeAttributes(mergeAttributes(parent, x), xs)
    case Nil => parent
  }
  
  def mergeAttributes(parent: List[AttributeLike],
      child: AttributeLike): List[AttributeLike] =
    if (!parent.exists(x => isSame(x, child))) parent ::: List(child)
    else parent.map (x =>
      if (isSame(x, child)) child match {
        // since OO's hierarchy does not allow base members to be ommited,
        // child overrides needs to be implemented some other way.
        case attr: AttributeDecl =>
          Some(x)
        case _ => Some(x)
      }
      else Some(x) ).flatten
      
  def isSame(lhs: AttributeLike, rhs: AttributeLike) =
    (resolveRef(lhs), resolveRef(rhs)) match {
      case (x: AnyAttributeDecl, y: AnyAttributeDecl) => true
      case (x: AttributeDecl, y: AttributeDecl) =>
        (x.name == y.name && x.namespace == y.namespace)
      case (x: AttributeGroupDecl, y: AttributeGroupDecl) =>
        (x.name == y.name && x.namespace == y.namespace)
      case _ => false
    }
    
  def toOptional(that: ElemDecl) =
    ElemDecl(that.namespace, that.name, that.typeSymbol,
      that.defaultValue, that.fixedValue, 0, that.maxOccurs, that.nillable, that.substitutionGroup, None)
    
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
