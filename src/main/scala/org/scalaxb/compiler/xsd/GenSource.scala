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
  val defaultSuperName = "org.scalaxb.rt.DataModel"
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val choiceWrapper = mutable.ListMap.empty[ComplexTypeDecl, ChoiceDecl]
  val interNamespaceChoiceTypes = mutable.ListBuffer.empty[XsTypeSymbol]
  var argNumber = 0
  val schemas = context.schemas.toList
  // val typeNames = context.typeNames(packageName)

  abstract class Cardinality
  object Optional extends Cardinality
  object Single extends Cardinality
  object Multiple extends Cardinality
  
  case class Param(name: String,
    typeSymbol: XsTypeSymbol,
    cardinality: Cardinality,
    attribute: Boolean) {
    
    override def toString(): String = cardinality match {
      case Single   => makeParamName(name) + ": " + buildTypeName(typeSymbol)
      case Optional => makeParamName(name) + ": Option[" + buildTypeName(typeSymbol) + "]"
      case Multiple => makeParamName(name) + ": Seq[" + buildTypeName(typeSymbol) + "]"
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
        
    for (choice <- choices)
      myprintAll(makeChoiceTrait(choice).child)
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
    case XsInterNamespace => defaultSuperName
    case XsAny => "String"
    case XsDataRecord => "org.scalaxb.rt.DataRecord[Any]"
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
      case elem@ElemDecl(_, _, XsAny, _, _, _, _) =>
        types += (elem -> XsString)
      case elem@ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _) =>
        types += (elem -> symbol)
      case elem@ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _) =>
        types += (elem -> baseType(decl))
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem match {
          case elem@ElemDecl(_, _, XsAny, _, _, _, _) =>
            types += (elem -> XsString)
          case ElemDecl(_, _, symbol: BuiltInSimpleTypeSymbol, _, _, _, _) =>
            types += (elem -> symbol)
          case ElemDecl(_, _, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _) =>
            types += (elem -> baseType(decl))
          case _ => // do nothing
        }
      case _ => // do nothing
    }
    types
  }
  
  def containsForeignType(particles: List[Decl]) =
    particles.exists(_ match {
        case ref: ElemRef => ref.namespace != schema.targetNamespace
        case _ => false
      }
    )

  def makeChoiceTrait(choice: ChoiceDecl): scala.xml.Node = {
    val name = makeTypeName(context.choiceNames(choice))
    val simpleTypeParticles = particlesWithSimpleType(choice.particles)
    val sequences = choice.particles partialMap {
      case seq: SequenceDecl => seq
    }
    
    var n = 0
    def sequenceNumber = {
      n += 1
      n
    }
    
    val sequenceWrappers = sequences.map(x =>
        ComplexTypeDecl(schema.targetNamespace,
          name + "Sequence" + sequenceNumber, false, false,
          ComplexContentDecl.fromCompositor(x, Nil), Nil))
    val hasForeign = containsForeignType(choice.particles)
        
    def makeFromXmlCaseEntry(elem: ElemDecl) = if (simpleTypeParticles.contains(elem)) {
      val symbol = simpleTypeParticles(elem)
      
      "case x: scala.xml.Elem if x.label == " + quote(elem.name) + " =>" + newline +
      indent(3) + "org.scalaxb.rt.DataRecord(" + quote(elem.name) + ", " + 
        buildArg(symbol, "x", None, None, 1, 1) + ")"
    } else {
      "case x: scala.xml.Elem if x.label == " + quote(elem.name) + " =>" + newline +
      indent(3) + "org.scalaxb.rt.DataRecord(" + quote(elem.name) + ", " + 
        buildTypeName(elem.typeSymbol) + ".fromXML(x))"  
    }
    
    val fromXmlCases = choice.particles partialMap {
      case elem: ElemDecl => makeFromXmlCaseEntry(elem)
      case ref: ElemRef   => makeFromXmlCaseEntry(buildElement(ref))
    }
    
    if (fromXmlCases.size == 0)
      <source>
{ if (!hasForeign)
    "trait " + name + " extends " + defaultSuperName
}
object {name} {{  
  def fromXML = new PartialFunction[scala.xml.Node, org.scalaxb.rt.DataRecord[Any]] {{
    def isDefinedAt(x : scala.xml.Node) = false
    def apply(x : scala.xml.Node): org.scalaxb.rt.DataRecord[Any] = error("Undefined")
  }}
}}      
</source>
    else <source>
{
  if (!hasForeign)
    "trait " + name + " extends " + defaultSuperName
}
object {name} {{  
  def fromXML: PartialFunction[scala.xml.NodeSeq, org.scalaxb.rt.DataRecord[Any]] = {{
    { fromXmlCases.mkString(newline + indent(2)) }
  }}
}}
{
  sequenceWrappers.map(x => makeCaseClassWithType(x.name, x))
}
</source>    
  }
      
  def makeTrait(decl: ComplexTypeDecl): scala.xml.Node = {
    val name = buildTypeName(decl)
    log("GenSource.makeTrait: emitting " + name)

    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    val defaultType = makeProtectedTypeName(decl, context)
    val superNames = buildSuperNames(decl) // buildSuperNamesForTrait
    
    val extendString = if (superNames.isEmpty)
      ""
    else
      " extends " + superNames.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) = {
      val localPart = buildTypeName(decl, true)
      val name = buildTypeName(decl)
      "case (" + quote(decl.namespace) + ", " + quote(localPart) + ") => " + name + ".fromXML(node)"
    }
        
    return <source>
trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param + ";"
  vals.mkString(newline + indent(1))}
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
    log("GenSource.makeCaseClassWithType: emitting " + name)
    
    val superNames: List[String] = if (context.baseToSubs.contains(decl))
      List(defaultSuperName, buildTypeName(decl))
    else
      buildSuperNames(decl)
      
    val childElements = flattenElements(decl, name)
    val attributes = flattenAttributes(decl)
    val list = List.concat[Decl](childElements, attributes)
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    
    def superNamesString = superNames.mkString(" with ")
    
    val hasSequenceParam = paramList.size == 1 &&
      paramList.head.cardinality == Multiple
    
    def paramsString = if (hasSequenceParam)
      makeParamName(paramList.head.name) + ": " + buildTypeName(paramList.head.typeSymbol) + "*"      
    else
      paramList.map(_.toString).mkString("," + newline + indent(1))
    
    def argsString = if (hasSequenceParam)
      argList.head + ": _*"
    else decl.content.content match {
      case SimpContRestrictionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: argList.drop(1)).
          mkString("," + newline + indent(3)) 
      case SimpContExtensionDecl(base: XsTypeSymbol, _) =>
        (buildArg(decl.content.asInstanceOf[SimpleContentDecl], base) :: argList.drop(1)).
          mkString("," + newline + indent(3)) 
                
      case _ => argList.mkString("," + newline + indent(3))
    }
    
    val namespaceString = if (decl.namespace == null)
      "null"
    else
      quote(decl.namespace)
    
    val childElemParams = paramList.filter(!_.attribute)
    
    def childString = decl.content.content match {
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
  def toXML(elementLabel: String): scala.xml.Node = {{
    var scope: scala.xml.NamespaceBinding = scala.xml.TopScope
    { scopeString(schema.scope).reverse.mkString(newline + indent(2)) }
    val node = toXML(elementLabel, scope)
    node match {{
      case elem: scala.xml.Elem => elem % new scala.xml.PrefixedAttribute("xsi", "type",
        { quote(typeNameString) }, elem.attributes)
      case _ => node
    }}
  }} 
</source>
    
    return <source>
case class {name}({paramsString}) extends {superNamesString} {{
  { if (!decl.name.contains('@'))
      makeToXml }  
  def toXML(elementLabel: String, scope: scala.xml.NamespaceBinding): scala.xml.Node = {{
    val prefix = scope.getPrefix({namespaceString})
    var attribute: scala.xml.MetaData  = scala.xml.Null
    { attributeString }
    
    scala.xml.Elem(prefix, elementLabel,
      attribute, scope,
      { childString })
  }}
}}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} =
    {name}({argsString}) 
}}
</source>    
  }
  
  def buildAttributeString(attr: AttributeLike): String = attr match {
    case ref: AttributeRef => buildAttributeString(buildAttribute(ref))
    case x: AttributeDecl  => buildAttributeString(x)
  }
  
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
    case XsAny => buildXMLStringForSimpleType(param)
    case XsInterNamespace => buildXMLStringForComplexType(param)
    case XsDataRecord => buildXMLStringForChoiceWrapper(param)
    case _ => error("GenSource#buildXMLString: " + param.toString +
      " Invalid type " + param.typeSymbol.getClass.toString + ": " + param.typeSymbol.toString)    
  }
  
  def buildXMLStringForChoiceWrapper(param: Param) = {
    val typeName = buildTypeName(param.typeSymbol)
    
    param.cardinality match {
      case Single =>
        makeParamName(param.name) + ".toXML(" +
          makeParamName(param.name) + ".key, scope)"
      case Optional =>
        makeParamName(param.name) + " match {" + newline +
        indent(5) + "case Some(x) => x.toXML(x.key, scope)" + newline +
        indent(5) + "case None => Seq()" + newline +
        indent(4) + "}"
      case Multiple =>
        makeParamName(param.name) + ".map(x => x.toXML(x.key, scope))"   
    }
  } 
  
  def buildXMLStringForComplexType(param: Param) = param.cardinality match {
    case Single =>
      makeParamName(param.name) + ".toXML(" + quote(param.name) + ", scope)"
    case Optional =>
      makeParamName(param.name) + " match {" + newline +
      indent(5) + "case Some(x) => x.toXML(" + quote(param.name) + 
        ", scope)" + newline +
      indent(5) + "case None => Seq()" + newline +
      indent(4) + "}"
    case Multiple =>
      makeParamName(param.name) + ".map(x => x.toXML(" + quote(param.name) + ", scope))"    
  }
  
  def buildXMLStringForSimpleType(param: Param) = param.cardinality match {
    case Single =>    
      "scala.xml.Elem(prefix, " + quote(param.name) + ", " +
      "scala.xml.Null, scope, scala.xml.Text(" + makeParamName(param.name)  + ".toString))"
    case Optional =>
      makeParamName(param.name) + " match {" + newline +
      indent(5) + "case Some(x) => Seq(scala.xml.Elem(prefix, " + quote(param.name) + ", " + 
        "scala.xml.Null, scope, scala.xml.Text(" + makeParamName(param.name)  + ".toString)))" + newline +
      indent(5) + "case None => Seq()" + newline +
      indent(4) + "}"
    case Multiple =>
      makeParamName(param.name) + ".map(x => scala.xml.Elem(prefix, " + quote(param.name) + ", " +
      "scala.xml.Null, scope, scala.xml.Text(x.toString)))"      
  }
  
  def buildParam(decl: Decl): Param = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case ref: AttributeRef => buildParam(buildAttribute(ref))
    case _ => error("GenSource: unsupported delcaration " + decl.toString)
  }
    
  def buildParam(elem: ElemDecl): Param = {
    val cardinality = if (elem.maxOccurs > 1)
      Multiple
    else if (elem.minOccurs == 0)
      Optional
    else
      Single
    
    val symbol = elem.typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (choiceWrapper.keysIterator.contains(decl))
          XsDataRecord
        else
          elem.typeSymbol
      case _ => elem.typeSymbol
    }
        
    Param(elem.name, symbol, cardinality, false)
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val cardinality = if (toMinOccurs(attr) == 0)
      Optional
    else
      Single
    
    Param(attr.name, attr.typeSymbol, cardinality, true)
  }
      
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl       => buildArg(elem)
    case attr: AttributeDecl  => buildArg(attr)
    case ref: AttributeRef    => buildArg(buildAttribute(ref))
    case _ => error("GenSource: unsupported delcaration " + decl.toString)
  }
  
  def buildArg(elem: ElemDecl): String = {
    val typeSymbol = elem.typeSymbol
    
    typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => buildArg(symbol,
        buildSelector(elem.name), elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs)
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildArg(decl,
        buildSelector(elem.name), elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs) 
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildArg(elem, decl)
      case symbol: ReferenceTypeSymbol =>
        if (symbol.decl == null)
          error("GenSource#buildArg: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with null decl")
        else    
          error("GenSource#buildArg: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with " + symbol.decl.toString)
      case XsAny => buildArg(XsString,
        buildSelector(elem.name), elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs)
            
      case _ => error("GenSource#buildArg: " + elem.toString +
        " Invalid type " + typeSymbol.getClass.toString + ": " + typeSymbol.toString)
    }
  }

  def buildArg(elem: ElemDecl, decl: ComplexTypeDecl): String = {
    val typeName = buildTypeName(elem.typeSymbol)
    
    if (choiceWrapper.keysIterator.contains(decl)) {
      val choice = choiceWrapper(decl)
      val choicePosition = context.choicePositions(choice)
      if (elem.maxOccurs > 1) {
        if (choicePosition == 0)
          "node.child.filter(" + typeName + ".fromXML.isDefinedAt(_)).map(x =>" + newline +
          indent(3) + typeName + ".fromXML(x)).toList"
        else
          "node.child.filter(_.isInstanceOf[scala.xml.Elem]).drop(" + choicePosition + ")." + newline +
          indent(4) + "filter(" + typeName + ".fromXML.isDefinedAt(_))." + newline +
          indent(4) + "map(x =>" + newline +
          indent(5) + typeName + ".fromXML(x)).toList"
      } else if (elem.minOccurs == 0) {
        "node.child.filter(_.isInstanceOf[scala.xml.Elem]).drop(" +
          choicePosition + ").headOption match {" + newline +
        indent(4) + "case Some(x) if " + typeName + ".fromXML.isDefinedAt(x) => " + newline +
        indent(5) + "Some(" + typeName + ".fromXML(x))" + newline +
        indent(4) + "case _ => None" + newline +
        indent(3) + "}"         
      } else {
        typeName + ".fromXML(node.child.filter(_.isInstanceOf[scala.xml.Elem])(" + choicePosition + "))"
      }
    } else {
      if (elem.maxOccurs > 1) {
        "(node \\ \"" + elem.name + "\").map(" + typeName + ".fromXML(_))" 
      } else if (elem.minOccurs == 0) {
        "(node \\ \"" + elem.name + "\").headOption match {" + newline +
        indent(4) + "case None    => None" + newline +
        indent(4) + "case Some(x) => Some(" +  typeName + ".fromXML(x))" + newline +
        indent(3) + "}"
      } else {
        typeName + ".fromXML((node \\ \"" + elem.name + "\").head)" 
      } // if-else
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
        toMinOccurs(attr), 1)
        
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, buildSelector(attr), attr.defaultValue, attr.fixedValue,
        toMinOccurs(attr), 1)    
    
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      error("GenSource: Attribute with complex type " + decl.toString)

    case _ => error("GenSource: unsupported type: " + attr.typeSymbol)
  }

  def buildArg(decl: SimpleTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int): String = decl.content match {  
    
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) =>
      buildArg(base, selector, defaultValue, fixedValue, minOccurs, maxOccurs)
    case SimpTypListDecl(itemType: BuiltInSimpleTypeSymbol) =>
      buildArg(itemType, selector + ".text.split(' ')", None, None, 0, Int.MaxValue)
    
    case _ => error("GenSource: Unsupported content " + decl.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl): String = content.content match {
    case SimpContRestrictionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    case SimpContExtensionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    
    case _ => error("GenSource: Unsupported content " + content.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case base: BuiltInSimpleTypeSymbol => buildArg(base, "node", None, None, 1, 1)
    case ReferenceTypeSymbol(ComplexTypeDecl(_, _, _, _, content: SimpleContentDecl, _)) =>
      buildArg(content)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, "node", None, None, 1, 1)
        
    case _ => error("GenSource: Unsupported type " + typeSymbol.toString)    
  }
  
  def buildSelector(attr: AttributeDecl): String =
    if (attr.global)
      buildSelector("@{" + attr.namespace + "}" + attr.name)
    else
      buildSelector("@" + attr.name)

  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  
  def buildArg(typeSymbol: BuiltInSimpleTypeSymbol, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int): String = {
    
    val (pre, post) = typeSymbol.name match {
      case "String"     => ("", "")
      case "javax.xml.datatype.Duration" => ("org.scalaxb.rt.Helper.toDuration(", ")")
      case "java.util.Calendar" => ("org.scalaxb.rt.Helper.toCalendar(", ")")
      case "Boolean"    => ("", ".toBoolean")
      case "Int"        => ("", ".toInt")
      case "Long"       => ("", ".toLong")
      case "Short"      => ("", ".toShort")
      case "Float"      => ("", ".toFloat")
      case "Double"     => ("", ".toDouble")
      case "Byte"       => ("", ".toByte")
      case "BigInt"     => ("BigInt(", ")")
      case "BigDecimal" => ("BigDecimal(", ")")
      case "java.net.URI" => ("org.scalaxb.rt.Helper.toURI(", ")")
      case "javax.xml.namespace.QName"
        => ("javax.xml.namespace.QName.valueOf(", ")")
      case "Array[String]" => ("", ".split(' ')")
      case "Array[Byte]" => ("org.scalaxb.rt.Helper.toByteArray(", ")")
      // case "HexBinary"  => 
      case _        => error("GenSource: Unsupported type " + typeSymbol.toString) 
    }
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      selector + ".headOption match {" + newline +
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
    } else {
      pre + selector + ".text" + post
    }    
  }
  
  def quote(value: String) = "\"" + value + "\""
  
  def buildSuperNames(decl: ComplexTypeDecl) = {
    val superName = buildSuperName(decl)
    if (superName == defaultSuperName)
      List(superName) ::: buildOptions(decl)
    else
      List(defaultSuperName, superName) ::: buildOptions(decl)
  }
  
  def buildSuperNamesForTrait(decl: ComplexTypeDecl) = {
    val superName = buildSuperName(decl)
    if (superName == defaultSuperName)
      buildOptions(decl)
    else
      List(superName) ::: buildOptions(decl)
  }
  
  def buildSuperName(decl: ComplexTypeDecl): String = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => buildTypeName(base)
      case _ => defaultSuperName // makeTypeName(decl.content.content.base.name) 
    }
  
  def buildOptions(decl: ComplexTypeDecl) = {
    val set = mutable.Set.empty[String]
    
    for (choice <- choices;
        particle <- choice.particles) particle match {
      case ElemDecl(_, _, symbol: ReferenceTypeSymbol, _, _, _, _) =>
        if (!interNamespaceChoiceTypes.contains(symbol) &&
            symbol.decl == decl)
          set += makeTypeName(context.choiceNames(choice))
      
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem.typeSymbol match {
          case symbol: ReferenceTypeSymbol =>
            if (!interNamespaceChoiceTypes.contains(symbol) &&
                symbol.decl == decl)
              set += makeTypeName(context.choiceNames(choice))
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
      val list = for (particle <- particles)
        yield particle match {
          case compositor2: HasParticle => flattenElements(compositor2, "")
          case elem: ElemDecl           => List(elem)
          case ref: ElemRef             => List(buildElement(ref))
          case any: AnyDecl             => Nil
        }
      list.flatten
    
    case AllDecl(particles: List[_], _, _) =>
      val list = for (particle <- particles)
        yield particle match {
          case compositor2: HasParticle => flattenElements(compositor2, "")
          case elem: ElemDecl           => List(toOptional(elem))
          case ref: ElemRef             => List(buildElement(ref))
        }
      list.flatten
    
    case choice: ChoiceDecl =>
      List(buildChoiceRef(choice, name))
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
        case Nil     => error("Element not found: {" + namespace + "}:" + name)
      }
  
  def buildElement(ref: ElemRef) = {
    val that = elements(ref.namespace, ref.name)
    
    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    ElemDecl(that.namespace, that.name, that.typeSymbol, that.defaultValue,
      that.fixedValue, ref.minOccurs, ref.maxOccurs)
  }
  
  def buildElement(decl: SimpleTypeDecl): ElemDecl = decl.content match {
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl)) => buildElement(base)
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => buildElement(base)
    case _ => error("GenSource: unsupported type: " + decl)
  }
  
  def buildElement(base: BuiltInSimpleTypeSymbol): ElemDecl = 
    ElemDecl(schema.targetNamespace, "value", base, None, None, 1, 1)
    
  def buildChoiceRef(choice: ChoiceDecl, parentName: String) = {    
    argNumber += 1
    val name = "arg" + argNumber 
    
    val symbol = new ReferenceTypeSymbol(makeTypeName(context.choiceNames(choice)))
    val decl = ComplexTypeDecl(schema.targetNamespace, symbol.name, false, false, null, Nil)

    choiceWrapper(decl) = choice
    
    if (containsForeignType(choice.particles))
      interNamespaceChoiceTypes += symbol
    
    symbol.decl = decl
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    typeNames(decl) = makeTypeName(context.choiceNames(choice))
    
    ElemDecl(schema.targetNamespace, name, symbol, None, None,
      choice.minOccurs, choice.maxOccurs)
  }
  
  def toOptional(that: ElemDecl) =
    ElemDecl(that.namespace, that.name, that.typeSymbol,
      that.defaultValue, that.fixedValue, 0, that.maxOccurs)
        
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
  
  def indent(indent: Int) = "  " * indent
  
  def log(msg: String) = logger.log(msg)
}
