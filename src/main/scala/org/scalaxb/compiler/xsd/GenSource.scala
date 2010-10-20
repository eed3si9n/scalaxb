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

package org.scalaxb.compiler.xsd

import org.scalaxb.compiler.{Logger, Config}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._

abstract class GenSource(val schema: SchemaDecl,
    val context: XsdContext,
    packageName: Option[String]) extends Parsers with XMLOutput {  
  type =>?[A, B] = PartialFunction[A, B]
  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val choices = schema.choices
  val MIXED_PARAM = "mixed"
  
  def run: Seq[Node] = {
    log("xsd: GenSource.run")
    
    val nodes = mutable.ListBuffer.empty[Node]
    nodes += makeSchemaComment
    packageName foreach { _ => nodes += makePackageName }
    nodes += makeImports
    nodes ++= schema.typeList.flatMap {
      case decl: ComplexTypeDecl =>      
        if (context.baseToSubs.keysIterator.contains(decl))
          (if (!decl.abstractValue) makeSuperType(decl).child else Nil) ++
          makeTrait(decl).child
        else makeType(decl).child
        
      case decl: SimpleTypeDecl =>
        if (containsEnumeration(decl)) makeEnumType(decl)
        else Nil 
    }
    
    nodes ++= (for ((sch, group) <- context.groups if sch == this.schema)
      yield makeGroup(group))    
    nodes ++= (for (group <- schema.topAttrGroups.valuesIterator)
      yield makeAttributeGroup(group))
    nodes
  }
      
  def makeSuperType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(makeProtectedTypeName(schema.targetNamespace, decl, context), decl)
      
  def makeType(decl: ComplexTypeDecl): scala.xml.Node = {
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
  
  def particlesWithSimpleType(particles: List[Decl]) = {
    val types = mutable.ListMap.empty[ElemDecl, BuiltInSimpleTypeSymbol]
    
    def addIfMatch(elem: ElemDecl) {
      elem.typeSymbol match {
        case symbol: BuiltInSimpleTypeSymbol => types += (elem -> symbol)
        case ReferenceTypeSymbol(decl: SimpleTypeDecl) => types += (elem -> baseType(decl))
        case _ =>
      }
    }
    
    particles collect {
      case elem: ElemDecl => addIfMatch(elem)
      case ref: ElemRef   => addIfMatch(buildElement(ref))
    }
    types
  }
       
  def makeTrait(decl: ComplexTypeDecl): scala.xml.Node = {
    val name = buildTypeName(decl)
    log("GenSource.makeTrait: emitting " + name)

    val childElements = if (decl.mixed) Nil
      else flattenElements(decl)
    val list = List.concat[Decl](childElements, buildAttributes(decl))
    val paramList = list.map { buildParam }
    val argList = list map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
      case x => buildArg(x)
    }
    val defaultType = makeProtectedTypeName(schema.targetNamespace, decl, context)
    val superNames = buildSuperNames(decl)
    
    val extendString = if (superNames.isEmpty) ""
    else " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) = {
      val name = buildTypeName(decl)
      "case (" + quoteNamespace(decl.namespace) + ", " + quote(decl.family) + ") => " + name + ".fromXML(node)"
    }
    
    def makeToXmlCaseEntry(decl: ComplexTypeDecl) = {
      val name = buildTypeName(decl)
      "case x: " + name + " => " + name + ".toXML(x, __namespace, __elementLabel, __scope)"
    }
    
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator
    
    return <source>
{ buildComment(decl) }trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param.toScalaCode
  vals.mkString(newline + indent(1))}
}}

object {name} extends rt.XMLWriter[{name}] {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def fromXML(seq: scala.xml.NodeSeq): {name} = seq match {{
    case node: scala.xml.Node =>     
      rt.Helper.instanceType(node) match {{
        {
          val cases = for (sub <- context.baseToSubs(decl))
            yield makeCaseEntry(sub)
          cases.mkString(newline + indent(4))        
        }
        { 
          if (!decl.abstractValue) "case _ => " + defaultType + ".fromXML(node)"
          else """case x => error("Unknown type: " + x)"""
        }
      }}
    case _ => error("fromXML failed: seq must be scala.xml.Node")
  }}
  
  def toXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = __obj match {{
    { val cases = for (sub <- context.baseToSubs(decl))
        yield makeToXmlCaseEntry(sub)
      cases.mkString(newline + indent(2))        
    }
    {
      if (!decl.abstractValue) "case x: " + defaultType + " => " + defaultType + ".toXML(x, __namespace, __elementLabel, __scope)"
      else """case _ => error("Unknown type: " + __obj)"""
    }
  }}
}}

{ if (decl.abstractValue) compositors map { makeCompositor }
  else Nil }
</source>    
  }
          
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    log("GenSource#makeCaseClassWithType: emitting " + name)
    
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
    val unmixedParserList = flatParticles map { buildParser(_, decl.mixed, decl.mixed) }
    val parserList = if (decl.mixed) buildTextParser :: (unmixedParserList flatMap { List(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList = ( 0 to parserList.size - 1) map { buildSelector }
    val accessors = generateAccessors(paramList, splitSequences(decl))
    log("GenSource#makeCaseClassWithType: generateAccessors" + accessors)
    
    val particleArgs = if (decl.mixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(flatParticles((i - 1) / 2), i)
        else buildArgForOptTextRecord(i) }
      else primary match {
        case Some(all: AllDecl) => flatParticles map { buildArgForAll }
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
      "Seq.concat(" + particleArgs.mkString("," + newline + indent(3)) + ")" +
        (if (attributeArgs.isEmpty) ""
        else "," + newline + indent(3) + attributeArgs.mkString("," + newline + indent(3)))
    else if (hasSequenceParam)
      particleArgs.head + ": _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: attributeArgs).
          mkString("," + newline + indent(3))
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: attributeArgs).
          mkString("," + newline + indent(3))
      case _ =>
        (particleArgs ::: attributeArgs).mkString("," + newline + indent(3))
    }
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def childString = if (decl.mixed) "__obj." + makeParamName(MIXED_PARAM) + 
      ".flatMap(x => rt.DataRecord.toXML(x, x.namespace, x.key, __scope).toSeq): _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _, _) => "scala.xml.Text(__obj.value.toString)"
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>   "scala.xml.Text(__obj.value.toString)"
      case _ =>  
        if (childElemParams.isEmpty) "Nil: _*"
        else if (childElemParams.size == 1)
          "(" + buildXMLString(childElemParams(0)) + "): _*"
        else childElemParams.map(x => 
          buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), "): _*")
    }
    
    def attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    
    def scopeString(scope: scala.xml.NamespaceBinding): List[String] =
      if (scope == null || scope.uri == null) Nil
      else {
        if (scope.prefix == null)
          ("__scope = scala.xml.NamespaceBinding(null, " + quote(scope.uri) +
            ", __scope)") :: scopeString(scope.parent)
        else
          ("__scope = scala.xml.NamespaceBinding(" + quote(scope.prefix) +
            ", " + quote(scope.uri) + ", __scope)") :: scopeString(scope.parent)
      }
    
    def getPrefix(namespace: Option[String], scope: scala.xml.NamespaceBinding): Option[String] =
      if (scope == null || scope.uri == null) None
      else
        if (scope.prefix != null && Some(scope.uri) == namespace) Some(scope.prefix)
        else getPrefix(namespace, scope.parent)
        
    def typeNameString = (getPrefix(schema.targetNamespace, schema.scope) map {
      _ + ":" } getOrElse { "" }) + decl.name
      
    val groups = filterGroup(decl)
    val objSuperNames: List[String] = "rt.ElemNameParser[" + name + "]" ::
      groups.map(groupTypeName)
    
    def makeObject = if (simpleFromXml)
<source>object {name} extends rt.XMLWriter[{name}] {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def fromXML(seq: scala.xml.NodeSeq): {name} = seq match {{
    case node: scala.xml.Node => {name}({argsString})
    case _ => error("fromXML failed: seq must be scala.xml.Node")
  }}
  
  { if (decl.isNamed) makeToXml }{ makeToXml2 }
}}
</source> else
<source>object {name} extends {objSuperNames.mkString(" with ")} {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }{ 
    if (decl.mixed) newline + "override def isMixed: Boolean = true"
    else ""}
  
  def parser(node: scala.xml.Node): Parser[{name}] =
    { parserList.mkString(" ~ ") } ^^
        {{ case { parserVariableList.mkString(" ~ " + newline + indent(3)) } => {name}({argsString}) }}
        
  { if (decl.isNamed) makeToXml }{ makeToXml2 }
}}
</source>
    
    def makeToXml = <source>def toXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String]): scala.xml.NodeSeq = {{
    var __scope: scala.xml.NamespaceBinding = scala.xml.TopScope
    { scopeString(schema.scope).reverse.mkString(newline + indent(2)) }
    val node = toXML(__obj, __namespace, __elementLabel, __scope)
    node match {{
      case elem: scala.xml.Elem =>
        elem % new scala.xml.PrefixedAttribute(__scope.getPrefix(rt.Helper.XSI_URL),
          "type",
          { quote(typeNameString) }, elem.attributes)
      case _ => node
    }}
  }}
  
  </source>
    
    def makeToXml2 = <source>def toXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String], __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {{
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { attributeString }
    scala.xml.Elem(rt.Helper.getPrefix(__namespace, __scope).orNull,
      __elementLabel getOrElse {{ error("missing element label.") }},
      attribute, __scope,
      { childString })
  }}
  
</source>
      
    return <source>
{ buildComment(decl) }case class {name}({paramsString}){extendString}{ if (accessors.size == 0) ""
else " {" + newline +
  indent(1) + accessors.mkString(newline + indent(1)) + newline +
  "}" + newline }

{ makeObject }
{ compositors map { makeCompositor } }
</source>    
  }
    
  def buildComment(p: Product) = p match {
    case decl: TypeDecl =>
      if (schema.typeToAnnotatable.contains(decl))
        makeAnnotation(schema.typeToAnnotatable(decl).annotation)
      else makeAnnotation(decl.annotation)
    case anno: Annotatable =>
      makeAnnotation(anno.annotation)
    case _ => ""
  }
  
  def makeCompositor(compositor: HasParticle) = {
    val name = makeTypeName(context.compositorNames(compositor))
    val hasForeign = containsForeignType(compositor)
    
    compositor match {
      case seq: SequenceDecl  =>
        makeSequence(seq)
      case choice: ChoiceDecl =>  
        makeChoice(choice)
      case _ =>
        <source>trait  {name}
        
</source>
    }
    
    // <source>{
    //   if (!hasForeign)
    //     "trait " + name
    // }
    // </source>    
  }
  
  def makeChoice(choice: ChoiceDecl) = {
    val name = makeTypeName(context.compositorNames(choice))
    
    def buildDataRecordXMLString(typeName: String): String =
      "case x: " + typeName + " => " + typeName + ".toXML(__obj, __namespace, __elementLabel, __scope)"

    def buildElemXMLString(typeName: String): String =
      "case x: " + typeName + " => " + typeName + ".toXML(x, __namespace, __elementLabel, __scope)"
    
    val particleTypes = ((choice.particles collect {
      case elem: ElemDecl => elem.typeSymbol
      case ref: ElemRef => buildElement(ref).typeSymbol
    }) collect {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) => decl
    }).toList.distinct
    
    val particleTypeNames = particleTypes.map(buildTypeName(_))
    val pruned: List[ComplexTypeDecl] = (particleTypes map (decl =>       
      if (flattenSuperNames(decl).exists(x => particleTypeNames.contains(x))) None
      else Some(decl)
    )).flatten
    
    val cases = (choice.particles collect {
      case ref: GroupRef =>
        val group = buildGroup(ref)
        val primary = primaryCompositor(group)
        buildDataRecordXMLString(makeTypeName(context.compositorNames(primary)))
      case group: GroupDecl =>
        val primary = primaryCompositor(group)
        buildDataRecordXMLString(makeTypeName(context.compositorNames(primary)))
      case x: HasParticle =>
        buildDataRecordXMLString(makeTypeName(context.compositorNames(x)))
    }) ::: (pruned map (x => buildElemXMLString(buildTypeName(x))))
    
    val superNames: List[String] = buildOptions(choice)
    val superString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
      
    <source>trait {name}{superString}

object {name} {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def toXML(__obj: rt.DataRecord[Any], __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {{
    __obj.value match {{
      { cases.distinct.mkString(newline + indent(2)) }
      case _ => rt.DataRecord.toXML(__obj, __namespace, __elementLabel, __scope)
    }}
  }}
}}

</source>
  }
  
  def makeCompositorImport(compositor: HasParticle) = compositor match {
    case seq: SequenceDecl =>
      val name = makeTypeName(context.compositorNames(seq))
      <source>import {name}._
  </source> 
    case _ => <source></source>
  }
  
  def makeSequence(seq: SequenceDecl) = {
    val name = makeTypeName(context.compositorNames(seq))
    val particles = flattenElements(schema.targetNamespace, name, seq)
    val paramList = particles.map { buildParam }
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute)
    val paramsString = if (hasSequenceParam)
        makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
      else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))
    val childString = if (paramList.isEmpty) "Nil"
      else if (paramList.size == 1)
        buildXMLString(paramList(0))
      else paramList.map(x => 
        buildXMLString(x)).mkString("Seq.concat(", "," + newline + indent(4), ")")
    val superNames: List[String] = buildOptions(seq)
    val superString = if (superNames.isEmpty) ""
      else " extends " + superNames.mkString(" with ")
    
    <source>{ buildComment(seq) }case class {name}({paramsString}){superString}

object {name} extends rt.XMLWriter[{name}] {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def toXML(__obj: rt.DataRecord[Any], __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = __obj.value match {{
    case x: {name} => toXML(x, __namespace, __elementLabel, __scope)
    case _ => error("Expected {name}")      
  }}
  
  def toXML(__obj: {name}, __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {{    
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { childString }
  }}
}}

</source>
  }
    
  def makeGroup(group: GroupDecl) = {
    val compositors = context.compositorParents.filter(
      x => x._2 == makeGroupComplexType(group)).keysIterator
    val name = makeTypeName(context.compositorNames(group))  
    val compositor = primaryCompositor(group)
    val param = buildParam(compositor)
    val wrapperParam = compositor match {
      case choice: ChoiceDecl => param
      case _ => Param(param.namespace, param.name, XsDataRecord(param.typeSymbol),
        param.cardinality, param.nillable, param.attribute)
    }
    val mixedParam = Param(param.namespace, param.name, XsDataRecord(XsAny),
      param.cardinality, param.nillable, param.attribute)
    
    val parser = buildCompositorParser(compositor, 1, 1, false, false)
    val wrapperParser = compositor match {
      case choice: ChoiceDecl => parser
      case _ => buildCompositorParser(compositor, 1, 1, false, true)
    }
    val mixedparser = buildCompositorParser(compositor, 1, 1, true, true)
    
    val groups = filterGroup(compositor)
    val superNames: List[String] = 
      if (groups.isEmpty) List("rt.AnyElemNameParser")
      else groups.map(groupTypeName(_))
    
    <source>{ buildComment(group) }trait {name} extends {superNames.mkString(" with ")} {{
  private val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def parse{name}: Parser[{param.baseTypeName}] =
    {parser}
  
  def parse{name}(wrap: Boolean): Parser[{wrapperParam.baseTypeName}] =
    {wrapperParser}
    
  def parsemixed{name}: Parser[Seq[{mixedParam.baseTypeName}]] =
    {mixedparser}
}}

{compositors map { makeCompositor } }
</source>
  }
  
  def makeAttributeGroup(group: AttributeGroupDecl) = {
    val name = buildTypeName(group)
    val attributes = buildAttributes(group.attributes)  
    val paramList = attributes.map { buildParam }
    val argList = attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(group)
        case x => buildArg(x) 
      }
    val paramsString =paramList.map(
      _.toScalaCode).mkString("," + newline + indent(1))
    val argsString = argList.mkString("," + newline + indent(3))  
    val attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    
    <source>{ buildComment(group) }case class {name}({paramsString})

object {name} {{
  val targetNamespace: Option[String] = { quote(schema.targetNamespace) }
  
  def fromXML(node: scala.xml.Node): {name} = {{
    {name}({argsString})
  }}
  
  def toAttribute(__obj: {name}, attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding) = {{
    var attribute: scala.xml.MetaData  = attr
    {attributeString}
    attribute
  }} 
}}

</source>
  }
  
  def makeEnumType(decl: SimpleTypeDecl) = {
    val name = buildTypeName(decl)
    val enums = filterEnumeration(decl)
    
    def makeEnum(enum: EnumerationDecl) =
      "case object " + buildTypeName(name, enum) + " extends " + name + 
      " { override def toString = " + quote(enum.value) + " }" + newline
    
    def makeCaseEntry(enum: EnumerationDecl) =
      indent(2) + "case " + quote(enum.value) + " => " + buildTypeName(name, enum) + newline
    
    enums match {
      case x :: Nil =>
<source>
case class {name}()

object {name} {{
  def fromXML(seq: scala.xml.NodeSeq): {name} = {name}() 
  def fromString(value: String): {name} = {name}()
}}
</source>    
      case _ =>
<source>trait {name}

object {name} {{
  def fromXML(seq: scala.xml.NodeSeq): {name} = fromString(seq.text)

  def fromString(value: String): {name} = value match {{
{ enums.map(e => makeCaseEntry(e)) }
  }}
}}

{ enums.map(e => makeEnum(e)) }
</source>
    }
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
  
  def buildOptions(comositor: HasParticle): List[String] = {
    val set = mutable.Set.empty[String]
    
    def addIfMatch(comp: HasParticle, choice: ChoiceDecl) {
      if (comp == comositor && !containsForeignType(choice))
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
    <source>// Generated by &lt;a href="http://scalaxb.org/"&gt;scalaxb&lt;/a&gt;.
{makeAnnotation(schema.annotation)}</source>
  
  def makeAnnotation(anno: Option[AnnotationDecl]) = anno match {
    case Some(annotation) =>
      "/** " +
      (for (doc <- annotation.documentations;
        x <- doc.any)
          yield x.toString).mkString + newline +
      "*/" + newline
    case None => ""    
  }
  
  def makePackageName = packageName match {
    case Some(x) => <source>package {x}
</source>
    case None    => error("GenSource: package name is missing")
  }
  
  def makeImports = <source>import org.scalaxb.rt
</source>
}
