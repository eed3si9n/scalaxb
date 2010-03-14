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

import org.scalaxb.compiler.{ScalaNames, Logger}
import scala.collection.mutable
import scala.collection.{Map}
import scala.xml._
import java.io.{PrintWriter}

class GenSource(schema: SchemaDecl,
    context: XsdContext,
    out: PrintWriter,
    packageName: Option[String],
    firstOfPackage: Boolean,
    logger: Logger) extends ScalaNames {  
  val topElems = schema.topElems
  val elemList = schema.elemList
  val types = schema.types
  val choices = schema.choices

  val newline = System.getProperty("line.separator")
  val defaultSuperName = "DataModel"
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val baseToSubs = mutable.ListMap.empty[ComplexTypeDecl, List[ComplexTypeDecl]]
  val choiceNames = mutable.ListMap.empty[ChoiceDecl, String]
  val choicePositions = mutable.ListMap.empty[ChoiceDecl, Int]
  val complexTypes = mutable.Set.empty[ComplexTypeDecl]
  val choiceWrapper = mutable.ListMap.empty[ComplexTypeDecl, ChoiceDecl]
  val interNamespaceChoiceTypes = mutable.ListBuffer.empty[XsTypeSymbol]
  var argNumber = 0
  var choiceNumber = 0
  val schemas = context.schemas.toList
  val typeNames = context.typeNames(packageName)

  abstract class Cardinality
  object Optional extends Cardinality
  object Single extends Cardinality
  object Multiple extends Cardinality
  
  case class Param(name: String,
    typeSymbol: XsTypeSymbol,
    cardinality: Cardinality) {
    
    override def toString(): String = cardinality match {
      case Single   => makeParamName(name) + ": " + buildTypeName(typeSymbol)
      case Optional => makeParamName(name) + ": Option[" + buildTypeName(typeSymbol) + "]"
      case Multiple => makeParamName(name) + ": Seq[" + buildTypeName(typeSymbol) + "]"
    }      
  }

  lazy val xmlAttrs = Map[String, AttributeDecl](
    ("lang" -> AttributeDecl(XML_URI, "lang", xsString, None, None, OptionalUse, true)),
    ("space" -> AttributeDecl(XML_URI, "space", xsString, None, None, OptionalUse, true)),
    ("base" -> AttributeDecl(XML_URI, "base", xsAnyURI, None, None, OptionalUse, true)),
    ("id" -> AttributeDecl(XML_URI, "id", xsID, None, None, OptionalUse, true))
  )
    
  def run {
    import scala.collection.mutable
    log("xsd: GenSource.run")
    
    if (packageName.isDefined)
      myprintAll(makePackageName.child)

    if (firstOfPackage)
      myprintAll(makeParentClass.child)
    
    for (elem <- elemList;
        val typeSymbol = elem.typeSymbol;
        if typeSymbol.name.contains("@");
        if typeSymbol.isInstanceOf[ReferenceTypeSymbol];
        val ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol];
        if ref.decl.isInstanceOf[ComplexTypeDecl];
        val decl = ref.decl.asInstanceOf[ComplexTypeDecl]) {
      val name = makeTypeName(elem.name)
      if (!typeNames.valuesIterator.contains(name))
        typeNames(decl) = name
      else {
        var i = 2
        while (typeNames.valuesIterator.contains(name + i) || i > 100) {
          i += 1
        }
        typeNames(decl) = name + i
      }
      complexTypes += decl
    }
    
    for (typePair <- types;
        if typePair._2.isInstanceOf[ComplexTypeDecl];
        if !typePair._1.contains("@");
        val decl = typePair._2.asInstanceOf[ComplexTypeDecl]) {    
      typeNames(decl) = makeProtectedTypeName(decl)
      complexTypes += decl
    }
    
    for (typ <- complexTypes)  typ.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, base)
      case _ =>
    }
    
    for (typ <- complexTypes)
      if (baseToSubs.keysIterator.contains(typ))
        typeNames(typ) = makeTraitName(typ)
    
    makeChoiceNames()
    
    for (base <- baseToSubs.keysIterator)
      myprintAll(makeSuperType(base).child)
    
    for (base <- baseToSubs.keysIterator)
      myprintAll(makeTrait(base).child)
    
    for (typ <- complexTypes)
      if (!baseToSubs.keysIterator.contains(typ))
        myprintAll(makeType(typ).child)
        
    for (choice <- choices)
      myprintAll(makeChoiceTrait(choice).child)

    if (firstOfPackage)
      myprintAll(makeHelperObject.child)
  }
  
  def makeChoiceNames() {
    for (typ <- complexTypes) { 
      choiceNumber = 0
      typ.content.content match {
        case CompContRestrictionDecl(_, Some(compositor: HasParticle), _) =>
          makeChoiceName(compositor, typeNames(typ))
        case CompContExtensionDecl(_, Some(compositor: HasParticle), _) =>
          makeChoiceName(compositor, typeNames(typ))
        case _ =>
      }
    }
  }
  
  def makeChoiceName(compositor: HasParticle, name: String): Unit = compositor match {
    case SequenceDecl(particles: List[_], _, _) =>
      var index = 0
      for (particle <- particles) {
        particle match {
          case choice: ChoiceDecl =>
            makeChoiceName(choice, name)
            choicePositions(choice) = index
          case compositor2: HasParticle => makeChoiceName(compositor2, name)
          case _ =>
        }
        index += 1
      }
    
    case AllDecl(particles: List[_], _, _) =>
      for (particle <- particles)
        particle match {
          case compositor2: HasParticle => makeChoiceName(compositor2, name)
          case _ =>
        }
    
    case choice@ChoiceDecl(particles: List[_], _, _) =>
      if (choiceNumber == 0)
        choiceNames(choice) = name + "Option"
      else
        choiceNames(choice) = name + "Option" + (choiceNumber + 1)
      choiceNumber += 1
      choicePositions(choice) = 0
      
      for (particle <- particles)
        particle match {
          case compositor2: HasParticle => makeChoiceName(compositor2, name)
          case _ =>
        }
  }
  
  def makeTraitName(decl: ComplexTypeDecl) =
    if (decl.name.last == 'e')
      decl.name.dropRight(1) + "able"
    else
      decl.name + "able"
  
  def makeSuperType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(makeProtectedTypeName(decl), decl)
      
  def makeType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(typeNames(decl), decl)
  
  def associateSubType(subType: ComplexTypeDecl, base: ComplexTypeDecl) {
    if (baseToSubs.contains(base))
      baseToSubs(base) = subType :: baseToSubs(base)
    else
      baseToSubs(base) = subType :: Nil
  }
    
  def makeProtectedTypeName(decl: ComplexTypeDecl): String =
    if (!typeNames.valuesIterator.contains(decl.name))
      makeTypeName(decl.name)
    else {
      var name = makeTypeName(decl.name)  + "Type"
      for (i <- 2 to 100) {
        if (typeNames.valuesIterator.contains(name))
          name = makeTypeName(decl.name)  + "Type" + i
      }
      name
    }
  
  def makeTypeName(name: String) =
    if (name.contains("."))
      name
    else
      identifier(name).capitalize
  
  def makeParamName(name: String) =
    if (isKeyword(name))
      name + "Value"
    else
      identifier(name)
  
  def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")
  
  def buildTypeName(typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      if (!typeNames.contains(decl))
        error(schema.targetNamespace + ": Type name not found: " + decl.toString)

      typeNames(decl)
    case xsAny => defaultSuperName  
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
      case elem@ElemDecl(_, symbol: BuiltInSimpleTypeSymbol, _, _, _, _) =>
        types += (elem -> symbol)
      case elem@ElemDecl(_, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _) =>
        types += (elem -> baseType(decl))
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem match {
          case ElemDecl(_, symbol: BuiltInSimpleTypeSymbol, _, _, _, _) =>
            types += (elem -> symbol)
          case ElemDecl(_, ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _)), _, _, _, _) =>
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
    val name = makeTypeName(choiceNames(choice))
    val simpleTypes = particlesWithSimpleType(choice.particles)
    val hasForeign = containsForeignType(choice.particles)
    val targetType = if (hasForeign)
      defaultSuperName
    else
      name

    def buildWrapperName(elem: ElemDecl) =
      if (name.endsWith("Option"))
        name.dropRight(6) + makeTypeName(elem.name)
      else
        name + makeTypeName(elem.name)
      
    def wrap(elem: ElemDecl) = {
      val wrapperName = buildWrapperName(elem)
      val symbol = simpleTypes(elem)
      val mixin = if (hasForeign)
        ""
      else
        " with " + name

      "case class " + wrapperName + "(value: " + symbol.name +
        ") extends DataModel" + mixin + newline +
      newline +
      "object " + wrapperName + " {" + newline +
      "  def fromXML(node: scala.xml.Node) ="+ newline +
      "    " + wrapperName +
        "(" + buildArg(symbol, "node", None, None, 1, 1) + ")" + newline +
      "}" + newline
    }
    
    def makeCaseEntry(elem: ElemDecl) = {
      val typeName = if (simpleTypes.contains(elem))
        buildWrapperName(elem)
      else
        buildTypeName(elem.typeSymbol)
      "case x: scala.xml.Elem if x.label == " + quote(elem.name) + " => " +
        typeName + ".fromXML(x)"
    }
    
    return <source>
{ if (!hasForeign)
    "trait " + name }
object {name} {{  
  def fromXML: PartialFunction[scala.xml.Node, {targetType}] = {{
    {
      val cases = choice.particles partialMap {
        case elem: ElemDecl => makeCaseEntry(elem)
        case ref: ElemRef   => makeCaseEntry(buildElement(ref))
      }
      cases.mkString(newline + indent(2))        
    }
  }}
}}

{         
  simpleTypes.keysIterator.toList.map(wrap(_)).mkString(newline)
}
</source>    
  }
      
  def makeTrait(decl: ComplexTypeDecl): scala.xml.Node = {
    val name = typeNames(decl)
    log("GenSource.makeTrait: emitting " + name)

    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    val superName = buildSuperName(decl)
    val defaultType = makeProtectedTypeName(decl)
    val options = buildOptions(decl)
    
    val extendString = if (options.isEmpty)
      ""
    else
      " extends " + options.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) = {
      val name = typeNames(decl)
      "case " + quote(name) + " => " + name + ".fromXML(node)"
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
    val withoutNS = typeName.drop(typeName.indexOf(":") + 1)
    
    withoutNS match {{
      {
        val cases = for (sub <- baseToSubs(decl))
          yield makeCaseEntry(sub)
        cases.mkString(newline + indent(3))        
      }
      case _ => {defaultType}.fromXML(node)
    }}
  }}
}}
</source>    
  }
        
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    log("GenSource.makeCaseClassWithType: emitting " + name)
    
    val superNames: List[String] = if (baseToSubs.contains(decl))
      List(defaultSuperName, typeNames(decl))
    else
      buildSuperNames(decl)
      
    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
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
    
    return <source>
case class {name}({paramsString}) extends {superNamesString} {{
}}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} =
    {name}({argsString}) 
}}
</source>    
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

    val symbol = if (interNamespaceChoiceTypes.contains(elem.typeSymbol))
      xsAny
    else
      elem.typeSymbol
    
    Param(elem.name, symbol, cardinality)
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val cardinality = if (toMinOccurs(attr) == 0)
      Optional
    else
      Single
    
    Param(attr.name, attr.typeSymbol, cardinality)
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
          error("GenSource: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with null decl")
        else    
          error("GenSource: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with " + symbol.decl.toString)
            
      case _ => error("GenSource: " + elem.toString +
        " Invalid type " + typeSymbol.getClass.toString + ": " + typeSymbol.toString)
    }
  }

  def buildArg(elem: ElemDecl, decl: ComplexTypeDecl): String = {
    val typeName = buildTypeName(elem.typeSymbol)
    
    if (choiceWrapper.keysIterator.contains(decl)) {
      val choice = choiceWrapper(decl)
      val choicePosition = choicePositions(choice)
      if (elem.maxOccurs > 1) {
        if (choicePosition == 0)
          "node.child.filter(" + typeName + ".fromXML.isDefinedAt(_)).map(" + newline +
          indent(4) + typeName + ".fromXML(_)).toList"
        else
          "node.child.filter(_.isInstanceOf[scala.xml.Elem]).drop(" +
          choicePosition + ")." + newline +
          indent(4) + "filter(" + typeName + ".fromXML.isDefinedAt(_))." + newline +
          indent(4) + "map(" + typeName + ".fromXML(_)).toList"
      } else if (elem.minOccurs == 0) {
        "node.child.filter(_.isInstanceOf[scala.xml.Elem]).drop(" +
          choicePosition + ").headOption match {" + newline +
        indent(4) + "case Some(x) if " + typeName +
          ".fromXML.isDefinedAt(x) => Some(" + typeName + ".fromXML(x))" + newline +
        indent(4) + "case _       => None" + newline +
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
    case ReferenceTypeSymbol(ComplexTypeDecl(_, content: SimpleContentDecl, _)) =>
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
      case "javax.xml.datatype.Duration" => ("Helper.toDuration(", ")")
      case "java.util.Calendar" => ("Helper.toCalendar(", ")")
      case "Boolean"    => ("", ".toBoolean")
      case "Int"        => ("", ".toInt")
      case "Long"       => ("", ".toLong")
      case "Short"      => ("", ".toShort")
      case "Float"      => ("", ".toFloat")
      case "Double"     => ("", ".toDouble")
      case "Byte"       => ("", ".toByte")
      case "BigInt"     => ("BigInt(", ")")
      case "BigDecimal" => ("BigDecimal(", ")")
      case "java.net.URI" => ("java.net.URI.create(", ")")
      case "javax.xml.namespace.QName"
        => ("javax.xml.namespace.QName.valueOf(", ")")
      case "Array[String]" => ("", ".split(' ')")
      case "Array[Byte]" => ("org.scalaxb.runtime.Base64.decodeBuffer(", ")")
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
  
  def buildSuperName(decl: ComplexTypeDecl): String = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => typeNames(base)
      case _ => defaultSuperName // makeTypeName(decl.content.content.base.name) 
    }
  
  def buildOptions(decl: ComplexTypeDecl) = {
    val set = mutable.Set.empty[String]
    
    for (choice <- choices;
        particle <- choice.particles) particle match {
      case ElemDecl(_, symbol: ReferenceTypeSymbol, _, _, _, _) =>
        if (!interNamespaceChoiceTypes.contains(symbol) &&
            symbol.decl == decl)
          set += makeTypeName(choiceNames(choice))
      
      case ref: ElemRef =>
        val elem = buildElement(ref)
        elem.typeSymbol match {
          case symbol: ReferenceTypeSymbol =>
            if (!interNamespaceChoiceTypes.contains(symbol) &&
                symbol.decl == decl)
              set += makeTypeName(choiceNames(choice))
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
      case res@CompContRestrictionDecl(xsAny, _, _) =>
        flattenElements(res.compositor, name)
      
      case CompContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) =>
        List(buildElement(symbol))
      case CompContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) =>
        List(buildElement(base))
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name)) :::
          flattenElements(ext.compositor, name)
      case ext@CompContExtensionDecl(xsAny, _, _) =>
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
    ElemDecl(that.name, that.typeSymbol, that.defaultValue, that.fixedValue,
      ref.minOccurs, ref.maxOccurs)
  }
  
  def buildElement(decl: SimpleTypeDecl): ElemDecl = decl.content match {
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl)) => buildElement(base)
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => buildElement(base)
    case _ => error("GenSource: unsupported type: " + decl)
  }
  
  def buildElement(base: BuiltInSimpleTypeSymbol): ElemDecl = 
    ElemDecl("value", base, None, None, 1, 1)
    
  def buildChoiceRef(choice: ChoiceDecl, parentName: String) = {    
    argNumber += 1
    val name = "arg" + argNumber 
    
    val symbol = new ReferenceTypeSymbol(makeTypeName(choiceNames(choice)))
    val decl = ComplexTypeDecl(symbol.name, null, Nil)

    choiceWrapper(decl) = choice
    
    if (containsForeignType(choice.particles))
      interNamespaceChoiceTypes += symbol
    
    symbol.decl = decl
    typeNames(decl) = makeTypeName(choiceNames(choice))
    
    ElemDecl(name, symbol, None, None, choice.minOccurs, choice.maxOccurs)
  }
  
  def toOptional(that: ElemDecl) = {
    ElemDecl(that.name, that.typeSymbol, that.defaultValue, that.fixedValue,
      0, that.maxOccurs)
  }
        
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
  
  def myprint(n: Node) = n match {
    case Text(s)          => out.print(s)
    case EntityRef("lt")  => out.print('<')
    case EntityRef("gt")  => out.print('>')
    case EntityRef("amp") => out.print('&')
    case atom: Atom[_]    => out.print(atom.text)
    case _                => log("error in xsd:run: encountered "
      + n.getClass() + " " + n.toString)
  }
  
  def makePackageName = packageName match {
    case Some(x) => <source>package {x}
</source>
    case None    => error("GenSource: package name is missing")
  }
  
  def makeParentClass = {
    <source>
abstract class {defaultSuperName}
</source>    
  }
  
  def makeHelperObject = {
    <source>
class Calendar extends java.util.GregorianCalendar {{
  override def toString: String = Helper.toString(this)
}}

object Calendar {{
  def apply(value: String): Calendar = Helper.toCalendar(value)
  def unapply(value: Calendar): Option[String] = Some(Helper.toString(value))
}}

object Helper {{
  lazy val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()

  def toCalendar(value: String) = {{
    val gregorian = typeFactory.newXMLGregorianCalendar(value).toGregorianCalendar
    val cal = new Calendar()

    for (i &lt;- 0 to java.util.Calendar.FIELD_COUNT - 1)
      if (gregorian.isSet(i))
        cal.set(i, gregorian.get(i))
    cal
  }}

  def toString(value: Calendar) = {{
    val xmlGregorian = typeFactory.newXMLGregorianCalendar(value)
    xmlGregorian.toString
  }}

  def toDuration(value: String) =
    typeFactory.newDuration(value)
}}
</source>    
  }

  def indent(indent: Int) = "  " * indent
  
  def log(msg: String) = logger.log(msg)
}
