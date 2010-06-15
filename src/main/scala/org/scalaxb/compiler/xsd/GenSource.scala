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
  
  case class Param(namespace: String,
    name: String,
    typeSymbol: XsTypeSymbol,
    cardinality: Cardinality,
    nillable: Boolean,
    attribute: Boolean) {
    
    def toScalaCode: String = {
      val base = buildTypeName(typeSymbol)
      val typeName = cardinality match {
        case Single   =>
          if (nillable) "Option[" + base + "]"
          else base
        case Optional => "Option[" + base + "]"
        case Multiple => 
          "Seq[" + base + "]"
          // if (nillable) "Option[Seq[" + base + "]]"
          // else "Seq[" + base + "]"
      }
      
      makeParamName(name) + ": " + typeName
    }
  }

  lazy val xmlAttrs = Map[String, AttributeDecl](
    ("lang" -> AttributeDecl(XML_URI, "lang", XsString, None, None, OptionalUse, true)),
    ("space" -> AttributeDecl(XML_URI, "space", XsString, None, None, OptionalUse, true)),
    ("base" -> AttributeDecl(XML_URI, "base", XsAnyURI, None, None, OptionalUse, true)),
    ("id" -> AttributeDecl(XML_URI, "id", XsID, None, None, OptionalUse, true))
  )
    
  def run {
    import scala.collection.mutable
    log("xsd: GenSource.run")
    
    if (packageName.isDefined)
      myprintAll(makePackageName.child)
    
    myprintAll(makeImports.child)
    
    for (base <- context.baseToSubs.keysIterator;
        if !base.abstractValue;
        if context.complexTypes.contains((schema, base)))
      myprintAll(makeSuperType(base).child)
    
    for (base <- context.baseToSubs.keysIterator;
        if context.complexTypes.contains((schema, base)))
      myprintAll(makeTrait(base).child)
    
    for ((sch, typ) <- context.complexTypes;
        if sch == schema;
        if !context.baseToSubs.keysIterator.contains(typ))
      myprintAll(makeType(typ).child)
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
          if schema.types.contains(name))
      yield schema.types(name)) match {
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
  }
  
  def buildTypeName(decl: ComplexTypeDecl, localOnly: Boolean = false): String = {
    val pkg = packageName(decl, context)
    val typeNames = context.typeNames(pkg)
    if (!typeNames.contains(decl))
      error(pkg + ": Type name not found: " + decl.toString)
    
    if (localOnly)
      typeNames(decl)
    else if (pkg == packageName(schema, context))
      typeNames(decl)
    else pkg match {
      case Some(x) => x + "." + typeNames(decl)
      case None => typeNames(decl)
    }
  }
  
  def buildTypeName(decl: SimpleTypeDecl): String = decl.content match {
    case x: SimpTypRestrictionDecl => buildTypeName(baseType(decl))
    case x: SimpTypListDecl => "Seq[" + buildTypeName(baseType(decl)) + "]"
    case _ => error("GenSource: Unsupported content " +  decl.content.toString)    
  }
  
  def baseType(decl: SimpleTypeDecl) = decl.content match {
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => base
    case SimpTypListDecl(itemType: BuiltInSimpleTypeSymbol) => itemType
    case _ => error("GenSource: Unsupported content " +  decl.content.toString)
  }

  def particlesWithSimpleType(particles: List[Decl]) = {
    val types = mutable.ListMap.empty[ElemDecl, BuiltInSimpleTypeSymbol]
    for (particle <- particles) particle match {
      case elem@ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _, _) =>
        types += (elem -> symbol)
      case elem@ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _, _) =>
        types += (elem -> baseType(decl))
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem match {
          case ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _, _) =>
            types += (elem -> symbol)
          case ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _, _) =>
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
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
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
      val localPart = buildTypeName(decl, true)
      val name = buildTypeName(decl)
      "case (" + quote(decl.namespace) + ", " + quote(localPart) + ") => " + name + ".fromXML(node)"
    }
        
    return <source>
trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param.toScalaCode
  vals.mkString(newline + indent(1))}
  
  def toXML(namespace: String, elementLabel: String,
    scope: scala.xml.NamespaceBinding): scala.xml.Node
}}

object {name} {{  
  def fromXML(node: scala.xml.Node): {name} = {{
    val typeName = (node \ "@{{http://www.w3.org/2001/XMLSchema-instance}}type").text    
    val namespace = if (typeName.contains(':'))
      node.scope.getURI(typeName.dropRight(typeName.length - typeName.indexOf(':')))
    else
      node.scope.getURI(null)
      
    val value = if (typeName.contains(':'))
      typeName.drop(typeName.indexOf(':') + 1)
    else
      typeName
    
    (namespace, value) match {{
      {
        val cases = for (sub <- context.baseToSubs(decl))
          yield makeCaseEntry(sub)
        cases.mkString(newline + indent(3))        
      }
      { 
        if (!decl.abstractValue)
          "case _ => " + defaultType + ".fromXML(node)"
        else
          """case _ => error("Unknown type: " + typeName)"""
      }
    }}
  }}
}}
</source>    
  }
        
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    log("GenSource#makeCaseClassWithType: emitting " + name)
    
    val superNames: List[String] = if (context.baseToSubs.contains(decl))
      List(buildTypeName(decl))
    else
      buildSuperNames(decl)
    
    val particles = flattenElements(decl, name)
    val childElements = particles ::: flattenMixed(decl)
    val attributes = flattenAttributes(decl)    
    val list = List.concat[Decl](childElements, attributes)
    val paramList = list.map(buildParam(_))
    val parserList = particles map(buildParser(_))
    val parserVariableList =  for (i <- 0 to particles.size - 1)
      yield "p" + (i + 1)
    val argList = (for (i <- 0 to particles.size - 1)
      yield buildArg(particles(i), i) ).toList ::: (if (decl.mixed)
        List(buildArgForMixed(particles))
      else
        Nil) ::: (attributes map {
        case any: AnyAttributeDecl => buildArgForAnyAttribute(decl)
        case x => buildArg(x) 
      })
    val compositors = context.compositorParents.filter(
      x => x._2 == decl).keysIterator
    val compositorsList = compositors map(makeCompositor(_))
    
    val extendString = if (superNames.isEmpty) ""
    else " extends " + superNames.mkString(" with ")
    
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!decl.mixed)
    
    def paramsString = if (hasSequenceParam)
      makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
    else
      paramList.map(_.toScalaCode).mkString("," + newline + indent(1))
    
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
      "mixed.map(x => x.toXML(x.namespace, x.key, scope)): _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _) => "scala.xml.Text(value.toString)"
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>   "scala.xml.Text(value.toString)"
      case _ =>  childElemParams.map(x => 
        buildXMLString(x)).mkString("Seq(", "," + newline + indent(4), ").flatten: _*")
    }
    
    def attributeString = attributes.map(x => buildAttributeString(x)).mkString(newline + indent(2))
    
    def scopeString(scope: scala.xml.NamespaceBinding): List[String] = {
      if (scope == null || scope.uri == null)
        Nil
      else {
        if (scope.prefix == null)
          ("scope = scala.xml.NamespaceBinding(null, " + quote(scope.uri) +
            ", scope)") :: scopeString(scope.parent)
        else
          ("scope = scala.xml.NamespaceBinding(" + quote(scope.prefix) +
            ", " + quote(scope.uri) + ", scope)") :: scopeString(scope.parent)
      }
    }
    
    def getPrefix(namespace: String, scope: scala.xml.NamespaceBinding): String = {
      if (scope == null || scope.uri == null)
        null
      else
        if (scope.prefix != null && scope.uri == namespace)
          scope.prefix
        else
          getPrefix(namespace, scope.parent)
    }
        
    def typeNameString =
      getPrefix(schema.targetNamespace, schema.scope) + ":" + decl.name
    
    def makeToXml = <source>
  def toXML(namespace: String, elementLabel: String): scala.xml.Node = {{
    var scope: scala.xml.NamespaceBinding = scala.xml.TopScope
    { scopeString(schema.scope).reverse.mkString(newline + indent(2)) }
    val node = toXML(namespace, elementLabel, scope)
    node match {{
      case elem: scala.xml.Elem => elem % new scala.xml.PrefixedAttribute("xsi", "type",
        { quote(typeNameString) }, elem.attributes)
      case _ => node
    }}
  }} 
</source>
        
    def makeObject = if (simpleFromXml || particles.isEmpty) <source>object {name} {{
  def fromXML(node: scala.xml.Node): {name} =
    {name}({argsString})
}}
</source> else <source>object {name} extends rt.ElemNameParser[{name}] {{
  val targetNamespace = { quote(schema.targetNamespace) }
    
  def parser(node: scala.xml.Node): Parser[{name}] =
    { parserList.mkString(" ~ " + newline + indent(3)) } ^^
        {{ case { parserVariableList.mkString(" ~ " + newline + indent(3)) } => {name}({argsString}) }}
}}
</source>
    
    return <source>
case class {name}({paramsString}){extendString} {{
  { if (!decl.name.contains('@'))
      makeToXml }  
  def toXML(namespace: String, elementLabel: String, scope: scala.xml.NamespaceBinding): scala.xml.Node = {{
    val prefix = scope.getPrefix(namespace)
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { attributeString }
    
    scala.xml.Elem(prefix, elementLabel,
      attribute, scope,
      { childString })
  }}
}}

{ makeObject }
{ compositorsList }
</source>    
  }
      
  def makeCompositor(compositor: HasParticle) = {
    val name = makeTypeName(context.compositorNames(compositor))
    val hasForeign = containsForeignType(compositor)
    
    compositor match {
      case seq: SequenceDecl    =>
        val parent = context.compositorParents(seq)
        val decl = ComplexTypeDecl(parent.namespace,
          name,
          false,
          parent.mixed,
          ComplexContentDecl.fromCompositor(seq, Nil),
          Nil)
        
        makeCaseClassWithType(name, decl)
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
  
  def buildAttributeString(attr: AttributeLike): String = attr match {
    case ref: AttributeRef => buildAttributeString(buildAttribute(ref))
    case x: AttributeDecl  => buildAttributeString(x)
    case any: AnyAttributeDecl => buildAttributeString(any)
  }
  
  def buildAttributeString(any: AnyAttributeDecl): String =
    "anyAttribute.foreach(x =>" + newline +
    indent(3) + "if (x.namespace == null)" + newline +
    indent(3) + "  attribute = scala.xml.Attribute(null, x.key, x.value, attribute)" + newline +
    indent(3) + "else" + newline +
    indent(3) + "  attribute = scala.xml.Attribute(scope.getPrefix(x.namespace), x.key, x.value, attribute))"
  
  def buildAttributeString(attr: AttributeDecl): String = {
    val namespaceString = if (attr.global)
      "scope.getPrefix(" + quote(attr.namespace) + ")"
    else
      "null"
    
    if (toMinOccurs(attr) == 0)
      makeParamName(attr.name) + " match {" + newline +
      indent(3) + "case Some(x: " + buildTypeName(attr.typeSymbol) + ") =>" + newline +
      indent(4) + "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) +
        ", x.toString, attribute)" + newline +
      indent(3) + "case None    =>" + newline +
      indent(2) + "}"
    else
      "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
      makeParamName(attr.name) + ".toString, attribute)"      
  }
  
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
        makeParamName(param.name) + ".toXML(" + makeParamName(param.name) + ".namespace, " +
          makeParamName(param.name) + ".key, scope)"
      case Optional =>
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(x.namespace, x.key, scope)" + newline +
        indent(5) + "case None => Nil" + newline +
        indent(4) + "}"
      case Multiple =>
        makeParamName(param.name) + ".map(x => x.toXML(x.namespace, x.key, scope))"   
    }
  } 
  
  def buildXMLStringForComplexType(param: Param) = param.cardinality match {
    case Single =>
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + "," + 
          quote(param.name) + ", scope)" + newline +
        indent(5) + "case None =>   Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Attribute(\"xsi\", \"nil\", \"true\", scala.xml.Null), scope, Seq(): _*))" + newline +
        indent(4) + "}"
      else
        makeParamName(param.name) + ".toXML(" + quote(param.namespace) + "," + 
          quote(param.name)  + ", scope)"
    case Optional =>
      makeParamName(param.name) + " match {" + newline +
      indent(5) + "case Some(x) => x.toXML(" + quote(param.namespace) + "," + 
        quote(param.name) + ", scope)" + newline +
      indent(5) + "case None => Nil" + newline +
      indent(4) + "}"
    case Multiple =>
      makeParamName(param.name) + ".map(x => x.toXML(" + quote(param.namespace) + "," + 
        quote(param.name) + ", scope))"    
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
    case x: SimpTypRestrictionDecl => buildToString(selector, baseType(decl))
    case x: SimpTypListDecl => selector + ".map(x => " + buildToString("x", baseType(decl)) + ").mkString(\" \")"
    case _ => error("GenSource#buildToString Unsupported content " +  decl.content.toString)    
  }
  
  def buildXMLStringForSimpleType(param: Param) = param.cardinality match {
    case Single =>    
      if (param.nillable)
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Null, scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
        indent(5) + "case None =>   Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
          "scala.xml.Attribute(\"xsi\", \"nil\", \"true\", scala.xml.Null), scope, Seq(): _*))" + newline +
        indent(4) + "}"
      else
        "scala.xml.Elem(prefix, " + quote(param.name) + ", " +
        "scala.xml.Null, scope, scala.xml.Text(" + buildToString(makeParamName(param.name), param.typeSymbol) + "))"
    case Optional =>
      makeParamName(param.name) + " match {" + newline +
      indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
        "scala.xml.Null, scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
      indent(5) + "case None => Seq()" + newline +
      indent(4) + "}"
    case Multiple =>
      makeParamName(param.name) + ".map(x => scala.xml.Elem(prefix, " + quote(param.name) + ", " +
      "scala.xml.Null, scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))"      
  }
  
  def buildParam(decl: Decl): Param = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case ref: AttributeRef => buildParam(buildAttribute(ref))
    case any: AnyAttributeDecl => buildParam(any)
    case _ => error("GenSource#buildParam: unsupported delcaration " + decl.toString)
  }
    
  def buildParam(elem: ElemDecl): Param = {
    val cardinality = if (elem.maxOccurs > 1) Multiple
    else if (elem.minOccurs == 0) Optional
    else Single
    
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
    
    val retval = Param(elem.namespace, elem.name, symbol, cardinality, nillable, false)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val cardinality = if (toMinOccurs(attr) == 0)
      Optional
    else
      Single
    
    val retval = Param(attr.namespace, attr.name, attr.typeSymbol, cardinality, false, true)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(any: AnyAttributeDecl): Param =
    Param(null, "anyAttribute", XsAnyAttribute, Multiple, false, true)
    
  def buildConverter(seq: SequenceDecl): String = {
    val name = makeTypeName(context.compositorNames(seq))
    val particles = buildParticles(seq)
    val parserVariableList =  for (i <- 0 to particles.size - 1)
      yield "p" + (i + 1)
    
    val argList = (for (i <- 0 to particles.size - 1)
      yield buildArg(particles(i), i) ).toList
    
    val paramList = particles.map(buildParam(_))
    
    val parent = context.compositorParents(seq)
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!parent.mixed)
    
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
  def buildParser(particle: Particle): String = particle match {
    case elem: ElemDecl       => buildParser(elem, elem.minOccurs, elem.maxOccurs)
    case ref: ElemRef         =>
      val elem = buildElement(ref)
      buildParser(elem, elem.minOccurs, elem.maxOccurs)
    case any: AnyDecl         => buildParser(any, any.minOccurs, any.maxOccurs)
    case choice: ChoiceDecl   => buildParser(choice, choice.minOccurs, choice.maxOccurs)
  }
  
  def buildParser(any: AnyDecl, minOccurs: Int, maxOccurs: Int): String =
    buildParserString("any", minOccurs, maxOccurs)
  
  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildParser(compositor: HasParticle, minOccurs: Int, maxOccurs: Int): String = compositor match {
    case seq: SequenceDecl    => buildParser(seq, minOccurs, maxOccurs)
    case choice: ChoiceDecl   => buildParser(choice, minOccurs, maxOccurs)
    case all: AllDecl         => buildParser(all, minOccurs, maxOccurs)
  }
  
  def buildParser(seq: SequenceDecl, minOccurs: Int, maxOccurs: Int): String = {
    val parserList = seq.particles.map(x => buildParser(x))
    val base = parserList.mkString("(", " ~ " + newline + indent(2), ")") + " ^^ " + newline +
    indent(3) + buildConverter(seq)
    buildParserString(base, minOccurs, maxOccurs)
  }
  
  def buildParser(all: AllDecl, minOccurs: Int, maxOccurs: Int): String = {
    val parserList = all.particles.map(x => buildParser(x))
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
  def buildParser(choice: ChoiceDecl, minOccurs: Int, maxOccurs: Int): String = {
    def buildChoiceParser(particle: Particle): String = particle match {
      case compositor: HasParticle =>
        buildParser(compositor, math.max(compositor.minOccurs, 1), 1)
      case elem: ElemDecl       =>
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
      case any: AnyDecl         =>
        "(" + buildParser(any, math.max(any.minOccurs, 1), 1) + " ^^ " + newline +
        indent(3) + buildConverter(XsAny, math.max(any.minOccurs, 1), 1) + ")"
      case ref: ElemRef         =>
        val elem = buildElement(ref)
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
    }
    
    val parserList = choice.particles filterNot(
      _.isInstanceOf[AnyDecl]) map(buildChoiceParser(_))
    val nonany = if (parserList.size > 0)
      parserList.mkString(" ||| " + newline + indent(2))
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
      minOccurs: Int, maxOccurs: Int): String = elem.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildParser(elem, decl, minOccurs, maxOccurs)
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
      minOccurs: Int, maxOccurs: Int): String =
    if (compositorWrapper.keysIterator.contains(decl)) {
      val compositor = compositorWrapper(decl)
      buildParser(compositor, minOccurs, maxOccurs)
    } else
      buildParserString(elem, minOccurs, maxOccurs)
  
  def buildParserString(elem: ElemDecl, minOccurs: Int, maxOccurs: Int): String = {
    val base = if (elem.namespace == schema.targetNamespace)
      "rt.ElemName(targetNamespace, " + quote(elem.name) + ")"
    else
      "rt.ElemName(" + quote(elem.namespace) + ", " + quote(elem.name) + ")"
    buildParserString(base, minOccurs, maxOccurs)
  }
  
  def buildParserString(base: String, minOccurs: Int, maxOccurs: Int) = {
    if (maxOccurs > 1)
      "rep(" + base + ")"
    else if (minOccurs == 0)
      "opt(" + base + ")"
    else
      "(" + base + ")"
  }
  
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl        => buildArg(elem, 0)
    case attr: AttributeDecl   => buildArg(attr)
    case ref: AttributeRef     => buildArg(buildAttribute(ref))
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
    case XsAny => buildArgForAny(elem.namespace, elem.name,
      elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs)
    
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
      
      if (elem.maxOccurs > 1)
        selector + ".toList"
      else
        selector
    } else {
      if (elem.maxOccurs > 1)
        selector + ".map(x => " + typeName + ".fromXML(x.node)).toList" 
      else if (elem.minOccurs == 0)
        selector + " match {" + newline +
        indent(4) + "case None    => None" + newline +
        indent(4) + "case Some(x) => Some(" +  typeName + ".fromXML(x.node))" + newline +
        indent(3) + "}"
      else
        elem.nillable match {
          case Some(true) =>
            "if (" + selector + ".nil) None" + newline +
            indent(3) + "else Some(" +  typeName + ".fromXML(" + selector + ".node))" + newline
          case _ => typeName + ".fromXML(" + selector + ".node)" 
        }
    } // if-else
  }
  
  def toMinOccurs(attr: AttributeDecl) = 
    if (attr.use == RequiredUse ||
      attr.fixedValue.isDefined ||
      attr.defaultValue.isDefined)
      1
    else
      0
  
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
    
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) =>
      buildArg(base, selector, defaultValue, fixedValue, minOccurs, maxOccurs, nillable)
    case SimpTypListDecl(itemType: BuiltInSimpleTypeSymbol) =>
      buildArg(itemType, selector + ".text.split(' ')", None, None, 0, Int.MaxValue, None)
    
    case _ => error("GenSource: Unsupported content " + decl.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl): String = content.content match {
    case SimpContRestrictionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    case SimpContExtensionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    
    case _ => error("GenSource: Unsupported content " + content.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case base: BuiltInSimpleTypeSymbol => buildArg(base, "node", None, None, 1, 1, None)
    case ReferenceTypeSymbol(ComplexTypeDecl(_, _, _, _, content: SimpleContentDecl, _)) =>
      buildArg(content)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, "node", None, None, 1, 1, None)
        
    case _ => error("GenSource: Unsupported type " + typeSymbol.toString)    
  }
  
  def buildSelector(pos: Int): String =
    "p" + (pos + 1)
  
  def buildSelector(attr: AttributeDecl): String =
    if (attr.global)
      buildSelector("@{" + attr.namespace + "}" + attr.name)
    else
      buildSelector("@" + attr.name)

  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  
  def buildArgForAnyAttribute(parent: ComplexTypeDecl): String = {
    val attributes = flattenAttributes(parent) collect {
      case attr: AttributeDecl   => attr
      case ref: AttributeRef     => buildAttribute(ref) 
    }
    
    def makeCaseEntry(attr: AttributeDecl) = if (attr.global)
      "case scala.xml.PrefixedAttribute(pre, key, value, _) if pre == elem.scope.getPrefix(" +
        quote(attr.namespace) + ") &&" + newline +
      indent(7) + "key == " + quote(attr.name) + " => Nil"
    else
      "case scala.xml.UnprefixedAttribute(key, value, _) if key == " + quote(attr.name) + " => Nil"
    
    "node match {" + newline +
    indent(4) + "case elem: scala.xml.Elem =>" + newline +
    indent(4) + "  (elem.attributes.toList) flatMap {" + newline +
    attributes.map(x => makeCaseEntry(x)).mkString(indent(6), newline + indent(6), newline) +
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
  
  def buildArgForAny(namespace: String, elementLabel: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int) = {
    
    val selector = "(node.child.collect { case elem: scala.xml.Elem => elem })"
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      selector + ".headOption match {" + newline +
      indent(4) + "case None    => " + noneValue + newline +
      indent(4) + "case Some(x) => " + someValue + newline +
      indent(3) + "}"
    
    if (maxOccurs > 1) {
      "(node.child.collect {" + newline +
      indent(3) + "case x: scala.xml.Elem =>" + newline +
      indent(3) + "  rt.DataRecord(x.scope.getURI(x.prefix), x.label, x) }).toList"
    } else if (minOccurs == 0) {
      buildMatchStatement("None",
        "Some(rt.DataRecord(x.scope.getURI(x.prefix), x.label, x))")
    } else if (defaultValue.isDefined) {
      buildMatchStatement("rt.DataRecord(" + newline +
        indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
        indent(4) + "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), " + newline +
        indent(4) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
        indent(4) + "scala.xml.Text(" + quote(defaultValue.get) + ")))",
        "rt.DataRecord(x.scope.getURI(x.prefix), x.label, x)")
    } else if (fixedValue.isDefined) {
      "rt.DataRecord(" + newline +
        indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
        indent(4) + "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), "  + newline +
        indent(4) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
        indent(4) + "scala.xml.Text(" + quote(fixedValue.get) + ")))"
    } else {
      buildMatchStatement("error(" + quote("required element is missing: " + elementLabel)  + ")",
        "rt.DataRecord(" + newline +
          indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", x)")
    }
  }
  
  def buildArg(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAny => selector
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, selector, None, None, 1, 1, None)
    case ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)) =>
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
    else
      Nil
    
    particles.collect {
      case elem: ElemDecl if !isAnyOrChoice(elem.typeSymbol) =>
        makeCaseEntry(elem)
      case ref: ElemRef if !isAnyOrChoice(buildElement(ref).typeSymbol) =>
        makeCaseEntry(buildElement(ref))
    } ::: makeListForAny   
  }
  
  def buildArg(typeSymbol: BuiltInSimpleTypeSymbol, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int, nillable: Option[Boolean]): String = {
    
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
    
    val optionSelector = if (selector contains("@"))
      selector + ".headOption"
    else
      selector
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      optionSelector + " match {" + newline +
      indent(4) + "case None    => " + noneValue + newline +
      indent(4) + "case Some(x) => " + someValue + newline +
      indent(3) + "}"
    
    if (maxOccurs > 1) {
      if (selector.contains("split("))
        selector + ".toList.map(" + pre + "_" + post + ")"
      else if (pre.contains("("))
        selector + ".toList.map(x => " + pre + "x.text" + post + ")"
      else
        selector + ".toList.map(" + pre + "_.text" + post + ")"
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
    
  def quote(value: String) = "\"" + value + "\""
  
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
      case ElemDecl(_, _, symbol: ReferenceTypeSymbol, _, _, _, _, _) =>
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
    
  def flattenElements(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    argNumber = 0
    
    decl.content.content match {
      case SimpContRestrictionDecl(symbol: BuiltInSimpleTypeSymbol, _) =>
        List(buildElement(symbol))
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _) =>
        List(buildElement(base))
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base, makeTypeName(base.name))
      
      case SimpContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _) =>
        List(buildElement(symbol))
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _) =>
        List(buildElement(base))
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        flattenElements(base, makeTypeName(base.name))
      
      // complex content means 1. has child elements 2. has attributes
      case CompContRestrictionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) =>
        List(buildElement(symbol))
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) =>
        List(buildElement(base))
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name))        
      case res@CompContRestrictionDecl(XsAny, _, _) =>
        flattenElements(res.compositor, name)
      
      case CompContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) =>
        List(buildElement(symbol))
      case CompContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) =>
        List(buildElement(base))
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name)) :::
          flattenElements(ext.compositor, name)
      case ext@CompContExtensionDecl(XsAny, _, _) =>
        flattenElements(ext.compositor, name)
        
      case _ => Nil
    }
  }
  
  def flattenElements(compositor: Option[HasParticle], name: String): List[ElemDecl] =
      compositor match {
    case None => Nil
    case Some(c) => flattenElements(c, name)
  }
  
  def flattenElements(compositor: HasParticle, name: String): List[ElemDecl] =
      compositor match {
    case SequenceDecl(particles: List[_], _, _) =>
      particles flatMap {
        case compositor2: HasParticle => flattenElements(compositor2, "")
        case elem: ElemDecl           => List(elem)
        case ref: ElemRef             => List(buildElement(ref))
        case any: AnyDecl             => List(buildAnyRef(any))        
      }
      
    case AllDecl(particles: List[_], _, _) =>
      particles flatMap {
        case compositor2: HasParticle => flattenElements(compositor2, "")
        case elem: ElemDecl           => List(toOptional(elem))
        case ref: ElemRef             => List(buildElement(ref))        
      }
          
    case choice: ChoiceDecl =>
      List(buildCompositorRef(choice))
  }
  
  def buildParticles(compositor: HasParticle): List[ElemDecl] =
    compositor.particles collect {
      case compositor2: HasParticle => buildCompositorRef(compositor)
      case elem: ElemDecl           => elem
      case ref: ElemRef             => buildElement(ref)
      case any: AnyDecl             => buildAnyRef(any)
    }
  
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
      ref.defaultValue, ref.fixedValue, ref.use, that.global)
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
      })
  }
  
  def buildElement(decl: SimpleTypeDecl): ElemDecl = decl.content match {
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl)) => buildElement(base)
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => buildElement(base)
    case _ => error("GenSource: unsupported type: " + decl)
  }
  
  def buildElement(base: BuiltInSimpleTypeSymbol): ElemDecl = 
    ElemDecl(schema.targetNamespace, "value", base, None, None, 1, 1, None)
  
  def buildCompositorRef(compositor: HasParticle) = {    
    argNumber += 1
    val name = "arg" + argNumber 
    
    val symbol = new ReferenceTypeSymbol(makeTypeName(context.compositorNames(compositor)))
    val decl = ComplexTypeDecl(schema.targetNamespace, symbol.name, false, false, null, Nil)
    
    compositorWrapper(decl) = compositor
    
    if (containsForeignType(compositor))
      interNamespaceCompositorTypes += symbol
    
    symbol.decl = decl
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    typeNames(decl) = makeTypeName(context.compositorNames(compositor))
    
    ElemDecl(schema.targetNamespace, name, symbol, None, None,
      compositor match {
        case choice: ChoiceDecl => (compositor.minOccurs :: compositor.particles.map(_.minOccurs)).min
        case _ => compositor.minOccurs
      },
      compositor match {
        case choice: ChoiceDecl => (compositor.maxOccurs :: compositor.particles.map(_.maxOccurs)).max
        case _ => compositor.maxOccurs
      },
      None)
  }
  
  def containsForeignType(compositor: HasParticle) =
    compositor.particles.exists(_ match {
        case ref: ElemRef => ref.namespace != schema.targetNamespace
        case _ => false
      }
    )
    
  def toOptional(that: ElemDecl) =
    ElemDecl(that.namespace, that.name, that.typeSymbol,
      that.defaultValue, that.fixedValue, 0, that.maxOccurs, that.nillable)
  
  def buildAnyRef(any: AnyDecl) =
    ElemDecl(INTERNAL_NAMESPACE, "any", XsAny, None, None,
      any.minOccurs, any.maxOccurs, None)
      
  def flattenMixed(decl: ComplexTypeDecl) = if (decl.mixed)
    List(ElemDecl(INTERNAL_NAMESPACE, "mixed", XsMixed, None, None, 0, Integer.MAX_VALUE, None))
  else
    Nil
    
  def flattenAttributes(decl: ComplexTypeDecl): List[AttributeLike] =
    decl.content.content.attributes ::: (decl.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) => 
        flattenAttributes(base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) =>
        flattenAttributes(base)
      case _ => List()
    })

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
