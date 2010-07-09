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

import org.scalaxb.compiler.{Logger}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._
import java.io.{PrintWriter}

class GenSource(schema: SchemaDecl,
    context: XsdContext,
    out: PrintWriter,
    packageName: Option[String],
    firstOfPackage: Boolean,
    logger: Logger) extends ContextProcessor(logger) {  
  type -->[A, B] = PartialFunction[A, B]
  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val choices = schema.choices
  val newline = System.getProperty("line.separator")
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val compositorWrapper = mutable.ListMap.empty[ComplexTypeDecl, HasParticle]
  val interNamespaceCompositorTypes = mutable.ListBuffer.empty[XsTypeSymbol]
  var argNumber = 0
  val schemas = context.schemas.toList
  val INTERNAL_NAMESPACE = "http://scalaxb.org/internal"

  abstract class Cardinality
  case object Optional extends Cardinality
  case object Single extends Cardinality
  case object Multiple extends Cardinality
  
  def toCardinality(minOccurs: Int, maxOccurs: Int) =
    if (maxOccurs > 1) Multiple
    else if (minOccurs == 0) Optional
    else Single
  
  case class Param(namespace: String,
    name: String,
    typeSymbol: XsTypeSymbol,
    cardinality: Cardinality,
    nillable: Boolean,
    attribute: Boolean) {
    
    def baseTypeName: String = buildTypeName(typeSymbol)
    
    def typeName: String = cardinality match {
      case Single   =>
        if (nillable) "Option[" + baseTypeName + "]"
        else baseTypeName
      case Optional => "Option[" + baseTypeName + "]"
      case Multiple => 
        if (nillable) "Seq[Option[" + baseTypeName + "]]"
        else "Seq[" + baseTypeName + "]"
    }      
    
    def toScalaCode: String =
      makeParamName(name) + ": " + typeName
  }

  lazy val xmlAttrs = Map[String, AttributeDecl](
    ("lang" -> AttributeDecl(XML_URI, "lang", XsString, None, None, OptionalUse, None, true)),
    ("space" -> AttributeDecl(XML_URI, "space", XsString, None, None, OptionalUse, None, true)),
    ("base" -> AttributeDecl(XML_URI, "base", XsAnyURI, None, None, OptionalUse, None, true)),
    ("id" -> AttributeDecl(XML_URI, "id", XsID, None, None, OptionalUse, None, true))
  )
    
  def run {
    import scala.collection.mutable
    log("xsd: GenSource.run")
    
    myprintAll(makeSchemaComment.child)
    
    if (packageName.isDefined)
      myprintAll(makePackageName.child)
    
    myprintAll(makeImports.child)
    
    schema.typeList.collect {
      case decl: ComplexTypeDecl =>      
        if (context.baseToSubs.keysIterator.contains(decl)) {
          if (!decl.abstractValue)  myprintAll(makeSuperType(decl).child)

          myprintAll(makeTrait(decl).child)
        }
        else myprintAll(makeType(decl).child)        
    }
      
    for ((sch, group) <- context.groups;
        if sch == schema)
      myprintAll(makeGroup(group).child)
    
    for (group <- schema.topAttrGroups.valuesIterator)
      myprintAll(makeAttributeGroup(group).child)
  }
      
  def makeSuperType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(makeProtectedTypeName(decl, context), decl)
      
  def makeType(decl: ComplexTypeDecl): scala.xml.Node = {
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    makeCaseClassWithType(typeNames(decl), decl)
  }
  
  def types(namespace: String, name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topTypes.contains(name))
      yield schema.topTypes(name)) match {
        case x :: xs => x
        case Nil     => error("Type not found: {" + namespace + "}:" + name)
      }
  
  def buildTypeName(typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsInterNamespace => "rt.DataRecord[Any]"
    case XsAny          => "rt.DataRecord[scala.xml.Node]"
    case XsDataRecord   => "rt.DataRecord[Any]"
    case XsMixed        => "rt.DataRecord[Any]"
    case XsAnyAttribute => "rt.DataRecord[String]"
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl)
    case symbol:AttributeGroupSymbol => buildTypeName(attributeGroups(symbol.namespace, symbol.name))
  }
  
  def buildTypeName(decl: ComplexTypeDecl, localOnly: Boolean = false): String = {
    val pkg = packageName(decl, context)
    val typeNames = context.typeNames(pkg)
    if (!typeNames.contains(decl))
      error(pkg + ": Type name not found: " + decl.toString)
    
    if (localOnly) typeNames(decl)
    else if (pkg == packageName(schema, context)) typeNames(decl)
    else pkg match {
      case Some(x) => x + "." + typeNames(decl)
      case None => typeNames(decl)
    }
  }
  
  def buildTypeName(decl: SimpleTypeDecl): String = decl.content match {
    case x: SimpTypRestrictionDecl => buildTypeName(baseType(decl))
    case x: SimpTypListDecl => "Seq[" + buildTypeName(baseType(decl)) + "]"
    case x: SimpTypUnionDecl => buildTypeName(baseType(decl))
  }
  
  def buildTypeName(group: AttributeGroupDecl): String =
    makeTypeName(group.name)
  
  def baseType(decl: SimpleTypeDecl): BuiltInSimpleTypeSymbol = decl.content match {
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => base
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(decl2@SimpleTypeDecl(_, _, _))) => baseType(decl2)
    case SimpTypListDecl(itemType: BuiltInSimpleTypeSymbol) => itemType
    case SimpTypListDecl(ReferenceTypeSymbol(decl2@SimpleTypeDecl(_, _, _))) => baseType(decl2)
    case SimpTypUnionDecl() => XsString
    
    case _ => error("GenSource: Unsupported content " +  decl.content.toString)
  }

  def particlesWithSimpleType(particles: List[Decl]) = {
    val types = mutable.ListMap.empty[ElemDecl, BuiltInSimpleTypeSymbol]
    for (particle <- particles) particle match {
      case elem@ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _, _, _) =>
        types += (elem -> symbol)
      case elem@ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _, _)), _, _, _, _, _, _) =>
        types += (elem -> baseType(decl))
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem match {
          case ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _, _, _) =>
            types += (elem -> symbol)
          case ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _, _)), _, _, _, _, _, _) =>
            types += (elem -> baseType(decl))
          case _ => // do nothing
        }
      case _ => // do nothing
    }
    types
  }
       
  def makeTrait(decl: ComplexTypeDecl): scala.xml.Node = {
    val name = buildTypeName(decl)
    log("GenSource.makeTrait: emitting " + name)

    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, buildAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list map {
      case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
      case x => buildArg(x)
    }
    val defaultType = makeProtectedTypeName(decl, context)
    val superNames = buildSuperNames(decl)
    
    val extendString = if (superNames.isEmpty) ""
    else " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) = {
      val localPart = if (decl.name.startsWith("complexType@")) decl.name.drop("complexType@".size)
      else decl.name
      
      val name = buildTypeName(decl)
      "case (" + quote(decl.namespace) + ", " + quote(localPart) + ") => " + name + ".fromXML(node)"
    }
    
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator
    
    return <source>
{ buildComment(decl) }trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param.toScalaCode
  vals.mkString(newline + indent(1))}
  
  def toXML(__namespace: String, __elementLabel: String,
    __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq
}}

object {name} {{
  def fromXML(seq: scala.xml.NodeSeq): {name} = seq match {{
    case node: scala.xml.Node =>     
      val typeName = (node \ "@{{http://www.w3.org/2001/XMLSchema-instance}}type").text    
      val namespace = if (typeName.contains(':'))
        node.scope.getURI(typeName.dropRight(typeName.length - typeName.indexOf(':')))
      else node.scope.getURI(null)
    
      val value = if (typeName.contains(':')) typeName.drop(typeName.indexOf(':') + 1)
      else typeName
    
      (namespace, value) match {{
        {
          val cases = for (sub <- context.baseToSubs(decl))
            yield makeCaseEntry(sub)
          cases.mkString(newline + indent(3))        
        }
        { 
          if (!decl.abstractValue) "case _ => " + defaultType + ".fromXML(node)"
          else """case _ => error("Unknown type: " + typeName)"""
        }
      }}
    case _ => error("fromXML failed: seq must be scala.xml.Node")
  }}
}}

{ if (decl.abstractValue) compositors map(makeCompositor(_, decl.mixed))
  else Nil }
</source>    
  }
        
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    log("GenSource#makeCaseClassWithType: emitting " + name)
    
    val superNames: List[String] = if (context.baseToSubs.contains(decl))
      List(buildTypeName(decl))
    else buildSuperNames(decl)
    
    val flatParticles = flattenElements(decl, name)
    // val particles = buildParticles(decl, name)
    val childElements = flatParticles ::: flattenMixed(decl)
    val attributes = buildAttributes(decl)    
    val list = List.concat[Decl](childElements, attributes)
    val paramList = list.map(buildParam(_))
    val parserList = flatParticles map(buildParser(_, decl.mixed))
    val parserVariableList =  for (i <- 0 to flatParticles.size - 1)
      yield "p" + (i + 1)
    val argList = (for (i <- 0 to flatParticles.size - 1)
      yield buildArg(flatParticles(i), i) ).toList ::: (if (decl.mixed)
        List(buildArgForMixed(flatParticles))
      else
        Nil) ::: (attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
        case x => buildArg(x) 
      })
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator
        
    val extendString = if (superNames.isEmpty) ""
    else " extends " + superNames.mkString(" with ")
    
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!decl.mixed)
    
    def paramsString = if (hasSequenceParam)
      makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
    else paramList.map(_.toScalaCode).mkString("," + newline + indent(1))
    
    val simpleFromXml: Boolean = decl.content.isInstanceOf[SimpleContentDecl]
    
    def argsString = if (hasSequenceParam)
      argList.head + ": _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: argList.drop(1)).
          mkString("," + newline + indent(3))
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: argList.drop(1)).
          mkString("," + newline + indent(3))
      case _ =>
        argList.mkString("," + newline + indent(3))
    }
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def childString = if (decl.mixed)
      "mixed.flatMap(x => x.toXML(x.namespace, x.key, __scope).toSeq): _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _) => "scala.xml.Text(value.toString)"
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>   "scala.xml.Text(value.toString)"
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
    
    def getPrefix(namespace: String, scope: scala.xml.NamespaceBinding): String =
      if (scope == null || scope.uri == null) null
      else
        if (scope.prefix != null && scope.uri == namespace) scope.prefix
        else getPrefix(namespace, scope.parent)
        
    def typeNameString =
      if (getPrefix(schema.targetNamespace, schema.scope)  == null) decl.name
      else getPrefix(schema.targetNamespace, schema.scope) + ":" + decl.name
    
    def makeToXml = <source>
  def toXML(__namespace: String, __elementLabel: String): scala.xml.NodeSeq = {{
    var __scope: scala.xml.NamespaceBinding = scala.xml.TopScope
    { scopeString(schema.scope).reverse.mkString(newline + indent(2)) }
    val node = toXML(__namespace, __elementLabel, __scope)
    node match {{
      case elem: scala.xml.Elem =>
        elem % new scala.xml.PrefixedAttribute(__scope.getPrefix(rt.Helper.XSI_URL),
          "type",
          { quote(typeNameString) }, elem.attributes)
      case _ => node
    }}
  }} 
</source>
    
    val groups = filterGroup(decl)
    val objSuperNames: List[String] = "rt.ElemNameParser[" + name + "]" ::
      groups.map(groupTypeName)
    
    def makeObject = if (simpleFromXml || flatParticles.isEmpty) <source>object {name} {{
  def fromXML(seq: scala.xml.NodeSeq): {name} = seq match {{
    case node: scala.xml.Node => {name}({argsString})
    case _ => error("fromXML failed: seq must be scala.xml.Node")
  }}
    
}}
</source> else <source>object {name} extends {objSuperNames.mkString(" with ")} {{
  val targetNamespace = { quote(schema.targetNamespace) }
    
  def parser(node: scala.xml.Node): Parser[{name}] =
    { parserList.mkString(" ~ " + newline + indent(3)) } ^^
        {{ case { parserVariableList.mkString(" ~ " + newline + indent(3)) } => {name}({argsString}) }}
}}
</source>
      
    return <source>
{ buildComment(decl) }case class {name}({paramsString}){extendString} {{
  { if (!decl.name.contains('@')) makeToXml }  
  def toXML(__namespace: String, __elementLabel: String, __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {{
    val prefix = __scope.getPrefix(__namespace)
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { attributeString }
    scala.xml.Elem(prefix, __elementLabel,
      attribute, __scope,
      { childString })
  }}
}}

{ makeObject }
{ compositors map(makeCompositor(_, decl.mixed)) }
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
  
  def makeCompositor(compositor: HasParticle, mixed: Boolean) = {
    val name = makeTypeName(context.compositorNames(compositor))
    val hasForeign = containsForeignType(compositor)
    
    compositor match {
      case seq: SequenceDecl    =>
        makeSequence(seq)
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
  
  def makeSequence(seq: SequenceDecl) = {
    val name = makeTypeName(context.compositorNames(seq))
    val particles = flattenElements(seq, name)
    val paramList = particles.map(buildParam(_))
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
    
    <source>{ buildComment(seq) }case class {name}({paramsString}) {{
  def toXML(__namespace: String, __elementLabel: String, __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {{
    val prefix = __scope.getPrefix(__namespace)
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { childString }
  }}
}}

</source>
  }
  
  // context.compositorNames contains the definition of GroupDecl,
  // while particle GroupDecl may differ in cardinality.
  def groupTypeName(group: GroupDecl) =
    makeTypeName(context.compositorNames(groups(group.namespace, group.name)))
  
  def makeGroup(group: GroupDecl) = {
    val compositors = context.compositorParents.filter(
      x => x._2 == makeGroupComplexType(group)).keysIterator
    val name = makeTypeName(context.compositorNames(group))  
    val compositor = primaryCompositor(group)
    val param = buildParam(compositor)
    val parser = buildParser(compositor, 1, 1, false)
    val groups = filterGroup(compositor)
    val superNames: List[String] = 
      if (groups.isEmpty) List("rt.AnyElemNameParser")
      else groups.map(groupTypeName(_))
    
    <source>{ buildComment(group) }trait {name} extends {superNames.mkString(" with ")} {{
  def parse{name}: Parser[{param.baseTypeName}] =
    {parser}
}}

{compositors map(makeCompositor(_, false))}
</source>
  }
  
  def makeAttributeGroup(group: AttributeGroupDecl) = {
    val name = buildTypeName(group)
    val attributes = buildAttributes(group.attributes)  
    val paramList = attributes.map(buildParam(_))
    val argList = attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(group)
        case x => buildArg(x) 
      }
    val paramsString =paramList.map(
      _.toScalaCode).mkString("," + newline + indent(1))
    val argsString = argList.mkString("," + newline + indent(3))  
    val attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    <source>{ buildComment(group) }case class {name}({paramsString}) {{
  
  def toAttribute(attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding) = {{
    var attribute: scala.xml.MetaData  = attr
    {attributeString}
    attribute
  }}
}}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} = {{
    {name}({argsString})
  }}  
}}

</source>
  }
  
  def buildAttributeString(attr: AttributeLike): String = attr match {
    case ref: AttributeRef => buildAttributeString(buildAttribute(ref))
    case x: AttributeDecl  => buildAttributeString(x)
    case any: AnyAttributeDecl => buildAttributeString(any)
    case group: AttributeGroupDecl => buildAttributeString(group)
  }
  
  def buildAttributeString(any: AnyAttributeDecl): String =
    "anyAttribute.foreach(x =>" + newline +
    indent(3) + "if (x.namespace == null) attribute = scala.xml.Attribute(null, x.key, x.value, attribute)" + newline +
    indent(3) + "else attribute = scala.xml.Attribute(__scope.getPrefix(x.namespace), x.key, x.value, attribute))"
  
  def buildAttributeString(attr: AttributeDecl): String = {
    val namespaceString = if (attr.global)
      "__scope.getPrefix(" + quote(attr.namespace) + ")"
    else "null"
    val name = makeParamName(buildParam(attr).name)
        
    if (toMinOccurs(attr) == 0)
      name + " match {" + newline +
      indent(3) + "case Some(x) =>" + newline +
      indent(4) + "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) +
        ", " + buildToString("x", attr.typeSymbol) + ", attribute)" + newline +
      indent(3) + "case None    =>" + newline +
      indent(2) + "}"
    else attr.defaultValue match {
      case Some(x) =>
        "if (" + buildToString(name, attr.typeSymbol) + " != " + quote(x) + ") " + newline +
        indent(3) + "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attribute)"
      case None =>
        "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attribute)"
    }
  }
  
  def buildAttributeString(group: AttributeGroupDecl): String =
    "attribute = " + makeParamName(buildParam(group).name) + ".toAttribute(attribute, __scope)"
  
  def buildXMLString(param: Param) = param.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildXMLStringForSimpleType(param)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildXMLStringForSimpleType(param)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildXMLStringForComplexType(param)
    case XsAny => buildXMLStringForChoiceWrapper(param)
    case XsInterNamespace => buildXMLStringForComplexType(param)
    case XsDataRecord => buildXMLStringForChoiceWrapper(param)
    case _ => error("GenSource#buildXMLString: " + param.toString +
      " Invalid type " + param.typeSymbol.getClass.toString + ": " + param.typeSymbol.toString)    
  }
  
  def buildXMLStringForChoiceWrapper(param: Param) = {
    val typeName = buildTypeName(param.typeSymbol)
    
    param.cardinality match {
      case Single =>
        if (param.nillable)
          makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + ", " +
            makeParamName(param.name) + ".key, __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"
        else
          makeParamName(param.name) + ".toXML(" + quote(param.namespace) + ", " +
            makeParamName(param.name) + ".key, __scope)"
      case Optional =>
        if (param.nillable)
          makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => x.toXML(x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"    
        else
          makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => x.toXML(x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => Nil" + newline +
          indent(4) + "}"
      case Multiple =>
        if (param.nillable)
          makeParamName(param.name) + ".flatMap(x => x match {" + newline +
          indent(5) + "case Some(x) => x.toXML(x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
            quote(param.name) + ", __scope)" + newline +
          indent(4) + "} )"        
        else  
          makeParamName(param.name) + ".flatMap(x => x.toXML(x.namespace, x.key, __scope))"
    }
  } 
  
  def buildXMLStringForComplexType(param: Param) = param.cardinality match {
    case Single =>
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + "," + 
          quote(param.name) + ", __scope)" + newline +
        indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope))" + newline +
        indent(4) + "}"
      else
        makeParamName(param.name) + ".toXML(" + quote(param.namespace) + "," + 
          quote(param.name)  + ", __scope)"
    case Optional =>
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + "," + 
          quote(param.name) + ", __scope)" + newline +
        indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope))" + newline +
        indent(4) + "}"    
      else
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope)" + newline +
        indent(5) + "case None => Nil" + newline +
        indent(4) + "}"
    case Multiple =>
      if (param.nillable)
        makeParamName(param.name) + ".flatMap(x => x match {" + newline +
        indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope)" + newline +
        indent(5) + "case None => rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope)" + newline +
        indent(4) + "} )"        
      else  
        makeParamName(param.name) + ".flatMap(x => x.toXML(" + quote(param.namespace) + "," + 
          quote(param.name) + ", __scope))"    
  }
  
  def buildToString(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol if (buildTypeName(symbol) == "java.util.GregorianCalendar") ||
      (buildTypeName(symbol) == "Array[Byte]")  =>
      "rt.Helper.toString(" + selector + ")"
    case symbol: BuiltInSimpleTypeSymbol => selector + ".toString"
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildToString(selector, decl)
    case _ => selector + ".toString"
  }
  
  def buildToString(selector: String, decl: SimpleTypeDecl): String = decl.content match {
    case x: SimpTypListDecl => selector + ".map(x => " + buildToString("x", baseType(decl)) + ").mkString(\" \")" 
    case _ => buildToString(selector, baseType(decl))
  }
  
  def buildXMLStringForSimpleType(param: Param) = param.cardinality match {
    case Single =>    
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
        indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope))" + newline +
        indent(4) + "}"
      else
        "scala.xml.Elem(prefix, " + quote(param.name) + ", " +
        "scala.xml.Null, __scope, scala.xml.Text(" + buildToString(makeParamName(param.name), param.typeSymbol) + "))"
    case Optional =>
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
        indent(5) + "case None => Seq(rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope))" + newline +
        indent(4) + "}"    
      else
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
        indent(5) + "case None => Nil" + newline +
        indent(4) + "}"
    case Multiple =>
      if (param.nillable)
        makeParamName(param.name) + ".map(x => x match {" + newline +
        indent(5) + "case Some(x) => scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + "))" + newline +
        indent(5) + "case None =>   rt.Helper.nilElem(" + quote(param.namespace) + ", " + 
          quote(param.name) + ", __scope)" + newline +
        indent(4) + "} )"
      else 
        makeParamName(param.name) + ".map(x => scala.xml.Elem(prefix, " + quote(param.name) + ", " +
        "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))"      
  }
  
  def buildParam(decl: Decl): Param = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case any: AnyAttributeDecl => buildParam(any)
    case group: AttributeGroupDecl => buildParam(group)
    case _ => error("GenSource#buildParam: unsupported delcaration " + decl.toString)
  }
  
  def buildParam(elem: ElemDecl): Param = {
    val symbol = elem.typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (compositorWrapper.keysIterator.contains(decl)) XsDataRecord
        else elem.typeSymbol
      case _ => elem.typeSymbol
    }
    val nillable = elem.nillable match {
      case None    => false
      case Some(x) => x
    }
    
    val retval = Param(elem.namespace, elem.name, symbol, 
      toCardinality(elem.minOccurs, elem.maxOccurs), nillable, false)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val cardinality = if (toMinOccurs(attr) == 0) Optional
    else Single
    val name = if (!attr.global) attr.name
    else schema.scope.getPrefix(attr.namespace) match {
      case null => attr.name
      case x => x + attr.name
    }
    
    val retval = Param(attr.namespace, name, attr.typeSymbol, cardinality, false, true)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(group: AttributeGroupDecl): Param = {
    val retval = Param(group.namespace, group.name,
      new AttributeGroupSymbol(group.namespace, group.name), Single, false, true)
    log("GenSource#buildParam:  " + retval)
    retval    
  }
  
  /// called by makeGroup
  def buildParam(compositor: HasParticle): Param =
    Param(null, "arg1", XsDataRecord,
    toCardinality(compositor.minOccurs, compositor.maxOccurs), false, false)
  
  def buildParam(any: AnyAttributeDecl): Param =
    Param(null, "anyAttribute", XsAnyAttribute, Multiple, false, true)
    
  def buildConverter(seq: SequenceDecl, mixed: Boolean): String = {
    val name = makeTypeName(context.compositorNames(seq))
    val particles = buildParticles(seq)
    val parserVariableList =  for (i <- 0 to particles.size - 1)
      yield "p" + (i + 1)
    
    val argList = (for (i <- 0 to particles.size - 1)
      yield buildArg(particles(i), i) ).toList
    
    val paramList = particles.map(buildParam(_))
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!mixed)
    
    def argsString = if (hasSequenceParam) argList.head + ": _*"
    else argList.mkString("," + newline + indent(3))
    
    "{ case " +
    parserVariableList.mkString(" ~ " + newline + indent(3)) + 
    " => rt.DataRecord(null, null, " + name + "(" + argsString + ")) }"
  }
  
  def buildConverter(seq: AllDecl): String = {
    "{ case p => foo()}"
  }
    
  def buildConverter(elem: ElemDecl): String =
    buildConverter(elem, elem.minOccurs, elem.maxOccurs)

  def buildConverter(elem: ElemDecl, minOccurs: Int, maxOccurs: Int): String =
    buildConverter(elem.typeSymbol, minOccurs, maxOccurs)
      
  def buildConverter(typeSymbol: XsTypeSymbol, minOccurs: Int, maxOccurs: Int): String =
    if (maxOccurs > 1)
      "(p => p.toList map(x => rt.DataRecord(x.namespace, x.name, " +
      buildArg("x.node", typeSymbol) + ")))"
    else if (minOccurs == 0)
      "(p => p match {" + newline +
      indent(3) + "case Some(x) => Some(rt.DataRecord(x.namespace, x.name, " +
      buildArg("x.node", typeSymbol) + "))" + newline +
      indent(3) + "case None => None })"
    else
      "(x => rt.DataRecord(x.namespace, x.name, " + buildArg("x.node", typeSymbol) + "))"
  
  // called by makeCaseClassWithType
  def buildParser(particle: Particle, mixed: Boolean): String = particle match {
    case elem: ElemDecl       => buildParser(elem, elem.minOccurs, elem.maxOccurs, mixed)
    case ref: ElemRef         =>
      val elem = buildElement(ref)
      buildParser(elem, elem.minOccurs, elem.maxOccurs, mixed)
    case compositor: HasParticle =>
      buildParser(compositor, compositor.minOccurs, compositor.maxOccurs, mixed)
    case any: AnyDecl =>
      buildParser(any, any.minOccurs, any.maxOccurs)
  }
  
  def buildParser(any: AnyDecl, minOccurs: Int, maxOccurs: Int): String =
    buildParserString("any", minOccurs, maxOccurs)
  
  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildParser(compositor: HasParticle,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = compositor match {
    case ref: GroupRef        =>
      val group = buildGroup(ref)
      buildParser(group, minOccurs, maxOccurs, mixed)
    case seq: SequenceDecl    => 
      if (containsSingleChoice(seq)) buildParser(singleChoice(seq), minOccurs, maxOccurs, mixed)
      else buildParser(seq, minOccurs, maxOccurs, mixed)
    case choice: ChoiceDecl   => buildParser(choice, minOccurs, maxOccurs, mixed)
    case all: AllDecl         => buildParser(all, minOccurs, maxOccurs, mixed)
    case group: GroupDecl     => buildParser(group, minOccurs, maxOccurs, false)
  }
  
  def primaryCompositor(group: GroupDecl): HasParticle =
    if (group.particles.size == 1) group.particles(0) match {
      case seq: SequenceDecl    => 
        if (containsSingleChoice(seq)) singleChoice(seq)
        else seq
      case choice: ChoiceDecl   => choice
      case all: AllDecl         => all  
    }
    else error("GenSource#primaryCompositor: group must contain one content model: " + group)
  
  def buildParser(group: GroupDecl, minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    val compositor = primaryCompositor(group)
    buildParserString("parse" + groupTypeName(group), 
      math.min(minOccurs, compositor.minOccurs),
      math.max(maxOccurs, compositor.maxOccurs) )
  }
  
  def buildParser(seq: SequenceDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    val parserList = seq.particles.map(x => buildParser(x, mixed))
    val base = parserList.mkString("(", " ~ " + newline + indent(2), ")") + " ^^ " + newline +
    indent(3) + buildConverter(seq, mixed)
    
    val retval = buildParserString(base, minOccurs, maxOccurs)
    log("GenSource#buildParser:  " + seq + newline + retval)
    retval
  }
  
  def buildParser(all: AllDecl, minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    val parserList = all.particles.map(x => buildParser(x, mixed))
    val base = parserList.mkString("(", " ~ " + newline + indent(2), ")") + " ^^ " + newline +
    indent(3) + buildConverter(all)
    buildParserString(base, minOccurs, maxOccurs)   
  }
  
  // one or more particles may be emptiable within the choices.
  // in such case, treat the whole choice to be minOccurs = 0,
  // but make sure all particles has at least minOccurs = 1.
  // additionally, treat all particles as maxOccurs = 1 and make the whole
  // choice repeatable in case any one particle is repeatable.
  // this may violate the schema, but it is a compromise as long as plurals are
  // treated as Seq[DataRecord].
  def buildParser(choice: ChoiceDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    def buildChoiceParser(particle: Particle): String = particle match {
      case ref: GroupRef        =>
        val group = buildGroup(ref)
        buildParser(group, math.max(group.minOccurs, 1), 1, false)
      case compositor: HasParticle =>
        buildParser(compositor, math.max(compositor.minOccurs, 1), 1, mixed)
      case elem: ElemDecl       =>
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1, mixed) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
      case any: AnyDecl         =>
        "(" + buildParser(any, math.max(any.minOccurs, 1), 1) + " ^^ " + newline +
        indent(3) + buildConverter(XsAny, math.max(any.minOccurs, 1), 1) + ")"
      case ref: ElemRef         =>
        val elem = buildElement(ref)
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1, mixed) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
    }
    
    val parserList = choice.particles filterNot(
      _.isInstanceOf[AnyDecl]) map(buildChoiceParser(_))
    val choiceOperator = if (choice.particles exists(_ match {
      case elem: ElemDecl => false
      case ref: ElemRef => false
      case _ => true
      })) "|||"
    else "|"
      
    val nonany = if (parserList.size > 0)
      parserList.mkString(" " + choiceOperator + " " + newline + indent(2))
    else ""
    
    val anyList = choice.particles filter(
      _.isInstanceOf[AnyDecl]) map(buildChoiceParser(_))
    val base = if (anyList.size > 0)
      if (nonany == "") anyList(0)
      else "(" + nonany + ") | " + newline +
        indent(2) + anyList(0)
    else nonany
    
    buildParserString(base, minOccurs, maxOccurs)
  }
  
  def buildParser(elem: ElemDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = elem.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildParser(elem, decl, minOccurs, maxOccurs, mixed)
    case XsAny => buildParserString("any", minOccurs, maxOccurs)
    case symbol: ReferenceTypeSymbol =>
      if (symbol.decl == null)
        error("GenSource#buildParser: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with null decl")
      else    
        error("GenSource#buildParser: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with " + symbol.decl.toString)
    case _ => error("GenSource#buildParser: " + elem.toString +
      " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
  }
  
  def buildParser(elem: ElemDecl, decl: ComplexTypeDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String =
    if (compositorWrapper.keysIterator.contains(decl)) {
      val compositor = compositorWrapper(decl)
      buildParser(compositor, minOccurs, maxOccurs, mixed)
    }
    else buildParserString(elem, minOccurs, maxOccurs)
  
  def buildParserString(elem: ElemDecl, minOccurs: Int, maxOccurs: Int): String = {
    val base = if (elem.namespace == schema.targetNamespace)
      "rt.ElemName(targetNamespace, " + quote(elem.name) + ")"
    else
      "rt.ElemName(" + quote(elem.namespace) + ", " + quote(elem.name) + ")"
    buildParserString(base, minOccurs, maxOccurs)
  }
  
  def buildParserString(base: String, minOccurs: Int, maxOccurs: Int) =
    if (maxOccurs > 1) "rep(" + base + ")"
    else if (minOccurs == 0) "opt(" + base + ")"
    else "(" + base + ")"
  
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl        => buildArg(elem, 0)
    case attr: AttributeDecl   => buildArg(attr)
    case ref: AttributeRef     => buildArg(buildAttribute(ref))
    case group: AttributeGroupDecl => buildArg(group)
    case _ => error("GenSource#buildArg unsupported delcaration " + decl.toString)
  }
    
  def buildArg(elem: ElemDecl, pos: Int): String = elem.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildArg(symbol,
      buildSelector(pos), elem.defaultValue, elem.fixedValue,
      elem.minOccurs, elem.maxOccurs, elem.nillable)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildArg(decl,
      buildSelector(pos), elem.defaultValue, elem.fixedValue,
      elem.minOccurs, elem.maxOccurs, elem.nillable) 
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildArg(elem, decl, buildSelector(pos))
    case XsAny => buildArgForAny(buildSelector(pos), elem.namespace, elem.name,
      elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs, elem.nillable)
    
    case symbol: ReferenceTypeSymbol =>
      if (symbol.decl == null)
        error("GenSource#buildArg: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with null decl")
      else    
        error("GenSource#buildArg: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with " + symbol.decl.toString)
    case _ => error("GenSource#buildArg: " + elem.toString +
      " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
  }

  def buildArg(elem: ElemDecl, decl: ComplexTypeDecl, selector: String): String = {
    val typeName = buildTypeName(elem.typeSymbol)
    
    if (compositorWrapper.keysIterator.contains(decl)) {
      val compositor = compositorWrapper(decl)
      
      if (elem.maxOccurs > 1) selector + ".toList"
      else selector
    } else {
      if (elem.maxOccurs > 1)
        elem.nillable match {
          case Some(true) =>
            selector + ".toList.map(x => if (x.nil) None" + newline +
            indent(3) + "else Some(" + typeName + ".fromXML(x.node)) )" 
          case _ =>
            selector + ".toList.map(x => " + typeName + ".fromXML(x.node))" 
        }
      else if (elem.minOccurs == 0)
        elem.nillable match {
          case Some(true) =>
            selector + " match {" + newline +
            indent(4) + "case Some(x) => if (x.nil) None" + newline +
            indent(4) + "  else Some(" +  typeName + ".fromXML(x.node))" + newline +
            indent(4) + "case None    => None" + newline +
            indent(3) + "}"
          case _ =>
            selector + " match {" + newline +
            indent(4) + "case Some(x) => Some(" +  typeName + ".fromXML(x.node))" + newline +
            indent(4) + "case None    => None" + newline +
            indent(3) + "}"
        }
      else
        elem.nillable match {
          case Some(true) =>
            "if (" + selector + ".nil) None" + newline +
            indent(3) + "else Some(" +  typeName + ".fromXML(" + selector + ".node))"
          case _ => typeName + ".fromXML(" + selector + ".node)" 
        }
    } // if-else
  }
  
  def buildArgForAny(selector: String, namespace: String, elementLabel: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int, nillable: Option[Boolean]) = {
    
    def buildMatchStatement(noneValue: String, someValue: String) = nillable match {
      case Some(true) =>        
        selector + " match {" + newline +
          indent(4) + "case Some(x) => if (x.nil) " + noneValue + newline +
          indent(4) + "  else " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}"
      case _ =>
        selector + " match {" + newline +
        indent(4) + "case Some(x) => " + someValue + newline +
        indent(4) + "case None    => " + noneValue + newline +
        indent(3) + "}"    
    }
        
    if (maxOccurs > 1) {
      nillable match {
        case Some(true) =>
          selector + ".toList.map(x => if (x.nil) None" + newline +
          indent(3) + "else Some(x.toDataRecord) )" 
        case _ =>
          selector + ".toList.map(_.toDataRecord)" 
      }
    } else if (minOccurs == 0) {
      buildMatchStatement("None",
        "Some(x.toDataRecord)")
    } else if (defaultValue.isDefined) {
      buildMatchStatement("rt.DataRecord(" + newline +
        indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
        indent(4) + "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), " + newline +
        indent(4) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
        indent(4) + "scala.xml.Text(" + quote(defaultValue.get) + ")))",
        "x.toDataRecord")
    } else if (fixedValue.isDefined) {
      "rt.DataRecord(" + newline +
        indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
        indent(4) + "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), "  + newline +
        indent(4) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
        indent(4) + "scala.xml.Text(" + quote(fixedValue.get) + ")))"
    }
    else selector + ".toDataRecord"
  }
  
  def toMinOccurs(attr: AttributeDecl) = 
    if (attr.use == RequiredUse ||
      attr.fixedValue.isDefined ||
      attr.defaultValue.isDefined) 1
    else 0
  
  def buildArg(attr: AttributeDecl): String = attr.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, buildSelector(attr), attr.defaultValue, attr.fixedValue,
        toMinOccurs(attr), 1, None)
        
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, buildSelector(attr), attr.defaultValue, attr.fixedValue,
        toMinOccurs(attr), 1, None)
        
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      error("GenSource: Attribute with complex type " + decl.toString)

    case _ => error("GenSource: unsupported type: " + attr.typeSymbol)
  }

  def buildArg(decl: SimpleTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int, nillable: Option[Boolean]): String = decl.content match {  
    
    case SimpTypRestrictionDecl(_) =>
      buildArg(baseType(decl), selector, defaultValue, fixedValue, minOccurs, maxOccurs, nillable)
    case SimpTypListDecl(_) =>
      buildArg(baseType(decl), selector, defaultValue, fixedValue, minOccurs, maxOccurs, nillable, true)
    case SimpTypUnionDecl() =>
      buildArg(baseType(decl), selector, defaultValue, fixedValue, minOccurs, maxOccurs, nillable)
    
    case _ => error("GenSource: Unsupported content " + decl.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl): String = content.content match {
    case SimpContRestrictionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    case SimpContExtensionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    
    case _ => error("GenSource: Unsupported content " + content.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case base: BuiltInSimpleTypeSymbol => buildArg(base, "node", None, None, 1, 1, None)
    case ReferenceTypeSymbol(ComplexTypeDecl(_, _, _, _, content: SimpleContentDecl, _, _)) =>
      buildArg(content)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, "node", None, None, 1, 1, None)
        
    case _ => error("GenSource: Unsupported type " + typeSymbol.toString)    
  }
  
  def buildSelector(pos: Int): String =
    "p" + (pos + 1)
  
  def buildSelector(attr: AttributeDecl): String =
    if (attr.global) buildSelector("@{" + attr.namespace + "}" + attr.name)
    else buildSelector("@" + attr.name)

  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  
  def buildArgForAnyAttribute(parent: ComplexTypeDecl): String =
    buildArgForAnyAttribute(flattenAttributes(parent))
  
  def buildArgForAnyAttribute(parent: AttributeGroupDecl): String =
    buildArgForAnyAttribute(flattenAttributes(parent.attributes))
  
  def buildArgForAnyAttribute(attributes: List[AttributeLike]): String = {
    def makeCaseEntry(attr: AttributeDecl) = if (attr.global)
      "case scala.xml.PrefixedAttribute(pre, key, value, _) if pre == elem.scope.getPrefix(" +
        quote(attr.namespace) + ") &&" + newline +
      indent(7) + "key == " + quote(attr.name) + " => Nil"
    else
      "case scala.xml.UnprefixedAttribute(key, value, _) if key == " + quote(attr.name) + " => Nil"
    
    "node match {" + newline +
    indent(4) + "case elem: scala.xml.Elem =>" + newline +
    indent(4) + "  (elem.attributes.toList) flatMap {" + newline +
    attributes.collect {
      case x: AttributeDecl => makeCaseEntry(x)
    }.mkString(indent(6), newline + indent(6), newline) +
    indent(4) + "    case scala.xml.UnprefixedAttribute(key, value, _) =>" + newline +
    indent(4) + "      List(rt.DataRecord(null, key, value.text))" + newline +
    indent(4) + "    case scala.xml.PrefixedAttribute(pre, key, value, _) =>" + newline +
    indent(4) + "      List(rt.DataRecord(elem.scope.getURI(pre), key, value.text))" + newline +
    indent(4) + "    case _ => Nil" + newline +
    indent(4) + "  }" + newline +
    indent(4) + "case _ => Nil" + newline +
    indent(3) + "}" 
  }
  
  def buildArgForMixed(particles: List[Decl]): String = {
    "(node.child.map {" + newline +
    indent(3) + fromXmlCases(particles, 3).mkString(newline + indent(3)) + newline +
    indent(3) + "case x: scala.xml.Text =>" + newline +
    indent(3) + "  rt.DataRecord(null, null, x.text)" + newline +
    indent(2) + "}).toList"
  }
    
  def buildArg(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAny => selector
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, selector, None, None, 1, 1, None)
    case ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _, _)) =>
      buildArg(baseType(decl), selector, None, None, 1, 1, None)
    case _ =>
      buildTypeName(typeSymbol) + ".fromXML(" + selector + ")"
  }
  
  def fromXmlCases(particles: List[Decl], indentBase: Int) = {
    def makeCaseEntry(elem: ElemDecl) =
      "case x: scala.xml.Elem if (x.label == " + quote(elem.name) + " && " + newline + 
        indent(indentBase + 2) + "x.scope.getURI(x.prefix) == " + quote(elem.namespace) + ") =>" + newline +
        indent(indentBase + 1) + "rt.DataRecord(x.scope.getURI(x.prefix), x.label, " +
        buildArg("x", elem.typeSymbol) + ")"
    
    def isAnyOrChoice(typeSymbol: XsTypeSymbol) = typeSymbol match {
      case XsAny => true
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) if compositorWrapper.keysIterator.contains(decl) =>
        true
      case _ => false
    }
    
    def makeListForAny = if (particles.exists(x => x.isInstanceOf[AnyDecl] ||
        (x.isInstanceOf[ElemDecl] && (isAnyOrChoice(x.asInstanceOf[ElemDecl].typeSymbol))) ||
        (x.isInstanceOf[ElemRef] && (isAnyOrChoice(buildElement(x.asInstanceOf[ElemRef]).typeSymbol)))  ))
      List("case x: scala.xml.Elem =>" + newline +
        indent(indentBase + 1) + "rt.DataRecord(x.scope.getURI(x.prefix), x.label, x)")
    else Nil
    
    particles.collect {
      case elem: ElemDecl if !isAnyOrChoice(elem.typeSymbol) =>
        makeCaseEntry(elem)
      case ref: ElemRef if !isAnyOrChoice(buildElement(ref).typeSymbol) =>
        makeCaseEntry(buildElement(ref))
    } ::: makeListForAny   
  }
  
  def buildArg(typeSymbol: BuiltInSimpleTypeSymbol, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int, nillable: Option[Boolean],
      list: Boolean = false): String = {
    
    val (pre, post) = typeSymbol.name match {
      case "String"     => ("", "")
      case "javax.xml.datatype.Duration" => ("rt.Helper.toDuration(", ")")
      case "java.util.GregorianCalendar" => ("rt.Helper.toCalendar(", ")")
      case "Boolean"    => ("", ".toBoolean")
      case "Int"        => ("", ".toInt")
      case "Long"       => ("", ".toLong")
      case "Short"      => ("", ".toShort")
      case "Float"      => ("", ".toFloat")
      case "Double"     => ("", ".toDouble")
      case "Byte"       => ("", ".toByte")
      case "BigInt"     => ("BigInt(", ")")
      case "BigDecimal" => ("BigDecimal(", ")")
      case "java.net.URI" => ("rt.Helper.toURI(", ")")
      case "javax.xml.namespace.QName"
        => ("javax.xml.namespace.QName.valueOf(", ")")
      case "Array[String]" => ("", ".split(' ')")
      case "Array[Byte]" => ("rt.Helper.toByteArray(", ")")
      // case "HexBinary"  => 
      case _        => error("GenSource#buildArg: Unsupported type " + typeSymbol.toString) 
    }
    
    val optionSelector = if (selector contains("@")) selector + ".headOption"
      else selector
    
    def buildMatchStatement(noneValue: String, someValue: String) = nillable match {
      case Some(true) =>
        optionSelector + " match {" + newline +
          indent(4) + "case Some(x) => if (x.nil) " + noneValue + newline +
          indent(4) + "  else " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}" 
      case _ =>
        optionSelector + " match {" + newline +
          indent(4) + "case Some(x) => " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}"      
    }
    
    if (list) {
      if (minOccurs == 0)
        buildMatchStatement("None", "Some(x.text.split(' ').toList.map(" + pre + "_" + post + ") )")
      else selector + ".text.split(' ').toList.map(" + pre + "_" + post + ")"
    } else if (maxOccurs > 1) {
      if (selector.contains("split("))
        selector + ".toList.map(" + pre + "_" + post + ")"
      else
        nillable match {
          case Some(true) =>
            selector + ".toList.map(x => if (x.nil) None" + newline +
            indent(3) + "else Some(" + pre + "x.text" + post + ") )"
          case _ => selector + ".toList.map(x => " + pre + "x.text" + post + ")"
        } 
    } else if (minOccurs == 0) {
      buildMatchStatement("None", "Some(" + pre + "x.text" + post + ")")
    } else if (defaultValue.isDefined) {
      buildMatchStatement(pre + quote(defaultValue.get) + post, pre + "x.text" + post)
    } else if (fixedValue.isDefined) {
      pre + quote(fixedValue.get) + post
    } else nillable match {
      case Some(true) => 
        "if (" + selector + ".nil) None" + newline +
        indent(3) + "else Some(" + pre + selector + ".text" + post + ")"
      case _ => pre + selector + ".text" + post
    }
  }
  
  def buildArg(group: AttributeGroupDecl): String = {
    val typeName = buildTypeName(group)
    typeName + ".fromXML(node)"
  }
  
  def quote(value: String) = if (value == null) "null"
    else "\"" + value + "\""
  
  def buildSuperNames(decl: ComplexTypeDecl) =
    buildSuperName(decl) ::: buildOptions(decl)
  
  def buildSuperName(decl: ComplexTypeDecl) = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => List(buildTypeName(base))
      case _ => List()
    }
  
  def buildOptions(decl: ComplexTypeDecl) = {
    val set = mutable.Set.empty[String]
    
    for (choice <- choices;
        particle <- choice.particles) particle match {
      case ElemDecl(_, _, symbol: ReferenceTypeSymbol, _, _, _, _, _, _) =>
        if (!interNamespaceCompositorTypes.contains(symbol) &&
            symbol.decl == decl)
          set += makeTypeName(context.compositorNames(choice))
      
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem.typeSymbol match {
          case symbol: ReferenceTypeSymbol =>
            if (!interNamespaceCompositorTypes.contains(symbol) &&
                symbol.decl == decl)
              set += makeTypeName(context.compositorNames(choice))
          case _ => 
        }

      case any: AnyDecl => // do nothing
      case _ => // do nothing
    }
        
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
  
  def flattenElements(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    argNumber = 0
    
    val build: ComplexTypeContent --> List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base, makeTypeName(base.name))
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base, makeTypeName(base.name))
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name))        
      case res@CompContRestrictionDecl(XsAny, _, _) =>
        flattenElements(res.compositor, name)
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name)) :::
          flattenElements(ext.compositor, name)
      case ext@CompContExtensionDecl(XsAny, _, _) =>
        flattenElements(ext.compositor, name)
        
      case _ => Nil
    }
    
    val pf = buildSimpleTypeRef orElse  build
    pf(decl.content.content)
  }
  
  def flattenElements(compositor: Option[HasParticle], name: String): List[ElemDecl] =
      compositor match {
    case None => Nil
    case Some(c) => flattenElements(c, name)
  }
  
  def flattenElements(compositor: HasParticle, name: String): List[ElemDecl] =
      compositor match {
    case ref:GroupRef =>
      flattenElements(buildGroup(ref), name)
      
    case group:GroupDecl =>
      List(buildCompositorRef(group))
    
    case SequenceDecl(particles: List[_], _, _) =>
      particles flatMap {
        case ref: GroupRef            => List(buildCompositorRef(buildGroup(ref)))
        case compositor2: HasParticle => List(buildCompositorRef(compositor2))
        case elem: ElemDecl           => List(elem)
        case ref: ElemRef             => List(buildElement(ref))
        case any: AnyDecl             => List(buildAnyRef(any))
      }
      
    case AllDecl(particles: List[_], _, _) =>
      particles flatMap {
        case ref: GroupRef            => List(buildCompositorRef(buildGroup(ref)))
        case compositor2: HasParticle => List(buildCompositorRef(compositor2))
        case elem: ElemDecl           => List(toOptional(elem))
        case ref: ElemRef             => List(buildElement(ref))    
      }
          
    case choice: ChoiceDecl =>
      List(buildCompositorRef(choice))
  }
  
  val buildSimpleTypeRef: ComplexTypeContent --> List[ElemDecl] = {
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
  
  def buildParticles(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    argNumber = 0
    
    val build: ComplexTypeContent --> List[ElemDecl] = {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
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
  
  def buildParticles(com: Option[HasParticle], name: String): List[ElemDecl] = com match {
    case Some(c) => buildParticles(c)
    case None => Nil
  }
  
  def buildParticles(compositor: HasParticle): List[ElemDecl] =
    compositor.particles map {
      case ref: GroupRef            =>
        val group = buildGroup(ref)
        buildCompositorRef(primaryCompositor(group))        
      case seq: SequenceDecl        =>
        if (containsSingleChoice(seq)) buildCompositorRef(singleChoice(seq))
        else buildCompositorRef(seq)
      case compositor2: HasParticle => buildCompositorRef(compositor)
      case elem: ElemDecl           => elem
      case ref: ElemRef             => buildElement(ref)
      case any: AnyDecl             => buildAnyRef(any)
    }
    
  def buildElement(decl: SimpleTypeDecl): ElemDecl = decl.content match {
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl)) => buildElement(base)
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => buildElement(base)
    case _ => error("GenSource: unsupported type: " + decl)
  }
    
  def buildCompositorRef(compositor: HasParticle) = {    
    argNumber += 1
    val name = "arg" + argNumber 
    
    val typeName = compositor match {
      case group: GroupDecl => groupTypeName(group)
      case _ => makeTypeName(context.compositorNames(compositor))
    }
    val symbol = new ReferenceTypeSymbol(typeName)
    val decl = ComplexTypeDecl(schema.targetNamespace, symbol.name,
      false, false, null, Nil, None)
    
    compositorWrapper(decl) = compositor
    
    if (containsForeignType(compositor))
      interNamespaceCompositorTypes += symbol
    
    symbol.decl = decl
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    typeNames(decl) = typeName
    
    ElemDecl(schema.targetNamespace, name, symbol, None, None,
      compositor match {
        case group: GroupDecl =>
          val primary = primaryCompositor(group)
          math.min(group.minOccurs, primary.minOccurs)
        case choice: ChoiceDecl => (compositor.minOccurs :: compositor.particles.map(_.minOccurs)).min
        case _ => compositor.minOccurs
      },
      compositor match {
        case group: GroupDecl =>
          val primary = primaryCompositor(group)
          math.max(group.maxOccurs, primary.maxOccurs)
        
        case choice: ChoiceDecl => (compositor.maxOccurs :: compositor.particles.map(_.maxOccurs)).max
        case _ => compositor.maxOccurs
      },
      None,
      None)
  }
  
  def containsForeignType(compositor: HasParticle) =
    compositor.particles.exists(_ match {
        case ref: ElemRef => ref.namespace != schema.targetNamespace
        case _ => false
      }
    )
        
  def flattenMixed(decl: ComplexTypeDecl) = if (decl.mixed)
    List(ElemDecl(INTERNAL_NAMESPACE, "mixed", XsMixed,
      None, None, 0, Integer.MAX_VALUE, None, None))
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
  
  def flattenAttributes(decl: ComplexTypeDecl): List[AttributeLike] =
    flattenAttributes(decl.content.content.attributes) ::: (
    decl.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) => 
        flattenAttributes(base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) =>
        flattenAttributes(base)
      case _ => Nil
    })
  
  // return a list of either AttributeDecl or AnyAttributeDecl
  def flattenAttributes(attributes: List[AttributeLike]): List[AttributeLike] =
    attributes flatMap {
      case any: AnyAttributeDecl => List(any)
      case attr: AttributeDecl => List(attr)
      case ref: AttributeRef   => List(buildAttribute(ref))
      case group: AttributeGroupDecl => flattenAttributes(group.attributes)
      case ref: AttributeGroupRef    => flattenAttributes(buildAttributeGroup(ref).attributes)
    }
  
  def toOptional(that: ElemDecl) =
    ElemDecl(that.namespace, that.name, that.typeSymbol,
      that.defaultValue, that.fixedValue, 0, that.maxOccurs, that.nillable, None)

  def buildAnyRef(any: AnyDecl) =
    ElemDecl(INTERNAL_NAMESPACE, "any", XsAny, None, None,
      any.minOccurs, any.maxOccurs, None, None)

      def attrs(namespace: String, name: String) =
        if (namespace == XML_URI)
          xmlAttrs(name)
        else
          (for (schema <- schemas;
                if schema.targetNamespace == namespace;
                if schema.topAttrs.contains(name))
              yield schema.topAttrs(name)) match {
              case x :: xs => x
              case Nil     => error("Attribute not found: {" + namespace + "}:" + name)
            }

  def buildAttribute(ref: AttributeRef) = {
    val that = attrs(ref.namespace, ref.name)
    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    AttributeDecl(that.namespace, that.name, that.typeSymbol,
      ref.defaultValue, ref.fixedValue, ref.use, that.annotation, that.global)
  }

  def elements(namespace: String, name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     => error("Element not found: {" + namespace + "}" + name)
      }

  def buildElement(ref: ElemRef) = {
    val that = elements(ref.namespace, ref.name)

    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    ElemDecl(that.namespace, that.name, that.typeSymbol, that.defaultValue,
      that.fixedValue, ref.minOccurs, ref.maxOccurs, ref.nillable match {
        case None => that.nillable
        case _    => ref.nillable
      }, that.annotation)
  }
  
  def buildElement(base: BuiltInSimpleTypeSymbol): ElemDecl = 
    ElemDecl(schema.targetNamespace, "value", base, None, None, 1, 1, None, None)
  
  def groups(namespace: String, name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topGroups.contains(name))
        yield schema.topGroups(name)) match {
        case x :: xs => x
        case Nil     => error("Group not found: {" + namespace + "}" + name)
      }
      
  def buildGroup(ref: GroupRef) = {
    val that = groups(ref.namespace, ref.name)
    
    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    GroupDecl(that.namespace, that.name, that.particles,
      ref.minOccurs, ref.maxOccurs, that.annotation)    
  }

  def attributeGroups(namespace: String, name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topAttrGroups.contains(name))
        yield schema.topAttrGroups(name)) match {
        case x :: xs => x
        case Nil     => error("Attribute group not found: {" + namespace + "}" + name)
      }
      
  def buildAttributeGroup(ref: AttributeGroupRef) =
    attributeGroups(ref.namespace, ref.name)
      
  def myprintAll(nodes: Seq[Node]) {
    for (node <- nodes)
      myprint(node)
  }
  
  def myprint(n: Node): Unit = n match {
    case Text(s)          => out.print(s)
    case EntityRef("lt")  => out.print('<')
    case EntityRef("gt")  => out.print('>')
    case EntityRef("amp") => out.print('&')
    case atom: Atom[_]    => out.print(atom.text)
    case elem: Elem       => myprintAll(elem.child)
    case _                => log("error in xsd:run: encountered "
      + n.getClass() + " " + n.toString)
  }
  
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
  
  def indent(indent: Int) = "  " * indent
  
  def log(msg: String) = logger.log(msg)
}
