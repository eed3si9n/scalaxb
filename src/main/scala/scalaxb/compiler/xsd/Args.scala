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
 
trait Args extends Params {
  // called by buildConverter
  def buildArg(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAny => selector
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, selector, None, None, Single, false)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(baseType(decl), selector, None, None, Single, false)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      buildFromXML(buildTypeName(typeSymbol), selector)
  }
    
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl        => buildArg(elem, 0)
    case attr: AttributeDecl   => buildArg(attr)
    case ref: AttributeRef     => buildArg(buildAttribute(ref))
    case group: AttributeGroupDecl => buildAttributeGroupArg(group)
    case _ => error("GenSource#buildArg unsupported delcaration " + decl.toString)
  }
  
  def buildArgForAll(elem: ElemDecl): String =
    buildArg(elem, buildSelector(elem))
  
  def buildArg(elem: ElemDecl, pos: Int): String =
    buildArg(elem, buildSelector(pos))
  
  def buildArg(elem: ElemDecl, selector: String): String =
    if ((isSubstitionGroup(elem))) selector
    else elem.typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => buildArg(symbol,
        selector, elem.defaultValue, elem.fixedValue,
        toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false))
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
        if (containsEnumeration(decl)) buildEnumArg(decl, selector,
          elem.defaultValue, elem.fixedValue,
          toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), false)
        else buildArg(decl,
          selector, elem.defaultValue, elem.fixedValue,
          toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false)) 
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildArg(elem, decl, selector)
      case XsAny => buildArgForAny(selector, elem.namespace, elem.name,
        elem.defaultValue, elem.fixedValue,
        toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false))
    
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
  
  def buildArg(elem: ElemDecl, decl: ComplexTypeDecl, selector: String): String =
    if (compositorWrapper.contains(decl)) {
      val compositor = compositorWrapper(decl)
      
      (toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse {false}) match {
        case (Multiple, _)    => selector + ".toSeq"
        case (Optional, true) => selector + " getOrElse { None }"
        case _ => selector
      }
    }
    else buildComplexTypeArg(decl, selector,
      elem.defaultValue, elem.fixedValue,
      toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false))
  
  def buildFromXML(typeName: String, selector: String): String =
    "fromXML[" + typeName + "](" + selector + ")"
  
  def buildToXML(typeName: String, args: String): String =
    "toXML[" + typeName + "](" + args + ")" 
  
  /// builds argument for non-compositor complex types.
  def buildComplexTypeArg(decl: ComplexTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean): String = {
    val typeName = buildTypeName(decl)
        
    val retval = (cardinality, nillable) match {
      case (Multiple, true) =>
        selector + ".toSeq map { x => if (x.nil) None else Some(" + buildFromXML(typeName, "x.node") + ") }"        
      case (Multiple, false) =>
        selector + ".toSeq map { x => " + buildFromXML(typeName, "x.node") + " }"
      case (Optional, true) =>
        selector + ".headOption map { x => if (x.nil) None else Some(" + buildFromXML(typeName, "x.node") + ") }"
      case (Optional, false) =>
        selector + ".headOption map { x => " + buildFromXML(typeName, "x.node") + " }"
      case (Single, true) =>
        "if (" + selector + ".nil) None else Some(" +  buildFromXML(typeName, selector + ".node") + ")"
      case (Single, false) =>
        buildFromXML(typeName, selector + ".node")      
    }

    log("GenSource#buildComplexTypeArg: " + typeName + ": " + retval)
    retval
  }
      
  def buildFromString(typeName: String, selector: String): String =
    typeName + ".fromString(" + selector + ")"
    
  def buildEnumArg(decl: SimpleTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean,
      list: Boolean): String = {    
    val typeName = buildTypeName(decl)
    val optionSelector = if (selector contains("@")) selector + ".headOption"
      else selector
    def buildSplitter(r: String) = "scalaxb.Helper.splitBySpace(" + r + ".text).toSeq.map(x => " +
      buildFromString(typeName, "x") + ")" 
    
    val retval = (list, cardinality, nillable) match {
      case (true, Multiple, true)     => selector + ".toSeq map { x => if (x.nil) None else Some(" + buildSplitter("x") + ") }"
      case (true, Multiple, false)    => selector + ".toSeq map { x => " + buildSplitter("x") + " }"
      case (true, Optional, true)     => selector + ".headOption map { x => if (x.nil) None else Some(" + buildSplitter("x") + ") }"
      case (true, Optional, false)    => selector + ".headOption map { x => " + buildSplitter("x") + " }"
      case (true, Single, true)       => "if (" + selector + ".nil) None else Some(" + buildSplitter(selector) + ")"
      case (true, Single, false)      => buildSplitter(selector)
      case (false, Multiple, true)    => selector + ".toSeq map { x => if (x.nil) None else Some(" + buildFromString(typeName, "x.text") + ") }"        
      case (false, Multiple, false)   => selector + ".toSeq map { x => " + buildFromString(typeName, "x.text") + " }"
      case (false, Optional, true)    => selector + ".headOption map { x => if (x.nil) None else Some(" + buildFromString(typeName, "x.text") + ") }"
      case (false, Optional, false)   => selector + ".headOption map { x => " + buildFromString(typeName, "x.text") + " }"
      case (false, Single, true)      => "if (" + selector + ".nil) None else Some(" +  buildFromString(typeName, selector + ".text") + ")"
      case (false, Single, false) =>
        fixedValue match {
          case Some(x) => buildFromString(typeName, quote(x))            
          case None => 
            defaultValue match {
              case Some(y) =>
                optionSelector + " match {" + newline +
                indent(4) + "  case Some(x) => " + buildFromString(typeName, "x.text") + newline +
                indent(4) + "  case None    => " + buildFromString(typeName, quote(y)) + newline +
                indent(4) + "}"
              case None => buildFromString(typeName, selector + ".text")
            }
        }      
    }
    
    log("GenSource#buildEnumArg: " + typeName + ": " + retval)
    retval
  }
    
  def buildArgForAny(selector: String, namespace: Option[String], elementLabel: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean) = {
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      if (nillable) selector + " match {" + newline +
          indent(4) + "  case Some(x) => if (x.nil) " + noneValue + " else " + someValue + newline +
          indent(4) + "  case None    => " + noneValue + newline +
          indent(4) + "}"
      else selector + " match {" + newline +
        indent(4) + "  case Some(x) => " + someValue + newline +
        indent(4) + "  case None    => " + noneValue + newline +
        indent(4) + "}"    
    
    def hardcoded(value: String) =
      "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), "  + newline +
      indent(5) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
      indent(5) + "scala.xml.Text(" + quote(value) + "))"
    
    val retval = (cardinality, nillable, defaultValue, fixedValue) match {
      case (Multiple, true, _, _) =>
        selector + ".toList.map { x => if (x.nil) None else Some(scalaxb.DataRecord(x)) }"
      case (Multiple, false, _, _) =>
        selector + ".toList.map { x => scalaxb.DataRecord(x) }"
      case (Optional, _, _, _) =>
        buildMatchStatement("None", "Some(scalaxb.DataRecord(x))")
      case (Single, _, _, Some(x)) =>
        "scalaxb.DataRecord(" + newline +
          indent(5) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
          indent(5) + hardcoded(x) + ")"
      case (Single, _, Some(x), _) =>
        buildMatchStatement("scalaxb.DataRecord(" + newline +
          indent(5) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
          indent(5) + hardcoded(x) + ")",
          "scalaxb.DataRecord(x)")        
      case (Single, false, _, _) =>
        "scalaxb.DataRecord(" + selector + ")"
    }
    
    retval
  }
    
  def buildArg(attr: AttributeDecl): String = attr.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, buildSelector(attr), attr.defaultValue, attr.fixedValue,
        toCardinality(toMinOccurs(attr), 1), false)
        
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, buildSelector(attr), attr.defaultValue, attr.fixedValue,
        toCardinality(toMinOccurs(attr), 1), false)
        
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      error("GenSource: Attribute with complex type " + decl.toString)

    case _ => error("GenSource: unsupported type: " + attr.typeSymbol)
  }

  def buildArg(decl: SimpleTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean): String = decl.content match {  
    
    case x: SimpTypRestrictionDecl =>
      if (containsEnumeration(decl)) buildEnumArg(decl, selector,
        defaultValue, fixedValue,
        cardinality, nillable, false)
      else buildArg(baseType(decl), selector, defaultValue, fixedValue, cardinality, nillable)
    case SimpTypListDecl(ReferenceTypeSymbol(itemType: SimpleTypeDecl)) 
          if containsEnumeration(itemType) =>
      buildEnumArg(itemType, selector,
        defaultValue, fixedValue,
        cardinality, nillable, true)
    case x: SimpTypListDecl =>
      buildArg(baseType(decl), selector, defaultValue, fixedValue, cardinality, nillable, true)
    case x: SimpTypUnionDecl =>
      buildArg(baseType(decl), selector, defaultValue, fixedValue, cardinality, nillable)
    
    case _ => error("GenSource: Unsupported content " + decl.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl): String = content.content match {
    case SimpContRestrictionDecl(base: XsTypeSymbol, _, _) => buildArg(content, base)
    case SimpContExtensionDecl(base: XsTypeSymbol, _) => buildArg(content, base)
    
    case _ => error("GenSource: Unsupported content " + content.content.toString)    
  }
  
  def buildArg(content: SimpleContentDecl, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case base: BuiltInSimpleTypeSymbol => buildArg(base, "node", None, None, Single, false)
    case ReferenceTypeSymbol(ComplexTypeDecl(_, _, _, _, _, content: SimpleContentDecl, _, _)) =>
      buildArg(content)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, "node", None, None, Single, false)
        
    case _ => error("GenSource: Unsupported type " + typeSymbol.toString)    
  }
  
  def buildSelector(elem: ElemDecl): String =
    if (elem.namespace == schema.targetNamespace) buildSelector(elem.name)
    else buildSelector((elem.namespace map { "{" + _ + "}" } getOrElse { "" })  + elem.name)
    
  def buildSelector(pos: Int): String =
    "p" + (pos + 1)
  
  def buildSelector(attr: AttributeDecl): String =
    if (attr.global) buildSelector("@" +
      (attr.namespace map { "{" + _ + "}" } getOrElse { "" }) + attr.name)
    else buildSelector("@" + attr.name)

  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  
  def buildArgForAnyAttribute(parent: ComplexTypeDecl): String =
    buildArgForAnyAttribute(flattenAttributes(parent))
  
  def buildArgForAnyAttribute(parent: AttributeGroupDecl): String =
    buildArgForAnyAttribute(flattenAttributes(parent.attributes))
  
  def buildArgForAnyAttribute(attributes: List[AttributeLike]): String = {
    def makeCaseEntry(attr: AttributeDecl) = if (attr.global)
      "case scala.xml.PrefixedAttribute(pre, key, value, _) if pre == elem.scope.getPrefix(" +
        (attr.namespace map { quote(_) } getOrElse { "null" }) + ") &&" + newline +
      indent(8) + "key == " + quote(attr.name) + " => Nil"
    else
      "case scala.xml.UnprefixedAttribute(key, value, _) if key == " + quote(attr.name) + " => Nil"
    
    "node match {" + newline +
    indent(5) + "case elem: scala.xml.Elem =>" + newline +
    indent(5) + "  (elem.attributes.toList) flatMap {" + newline +
    attributes.collect {
      case x: AttributeDecl => makeCaseEntry(x)
    }.mkString(indent(7), newline + indent(7), newline) +
    indent(5) + "    case scala.xml.UnprefixedAttribute(key, value, _) =>" + newline +
    indent(5) + "      List(scalaxb.DataRecord(None, Some(key), value.text))" + newline +
    indent(5) + "    case scala.xml.PrefixedAttribute(pre, key, value, _) =>" + newline +
    indent(5) + "      List(scalaxb.DataRecord(Option[String](elem.scope.getURI(pre)), Some(key), value.text))" + newline +
    indent(5) + "    case _ => Nil" + newline +
    indent(5) + "  }" + newline +
    indent(5) + "case _ => Nil" + newline +
    indent(4) + "}" 
  }
    
  def buildArgForMixed(particle: Particle, pos: Int): String =
    buildArgForMixed(particle, buildSelector(pos))
  
  def buildArgForMixed(particle: Particle, selector: String): String = {
    val cardinality = toCardinality(particle.minOccurs, particle.maxOccurs)
    val isCompositor = particle match {
      case ref: GroupRef => true
      case elem: ElemDecl =>
        elem.typeSymbol match {
          case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
            if (compositorWrapper.contains(decl)) true
            else false
          case _ => false 
        }
      case x: HasParticle => true
      case _ => false      
    }
    
    val retval = cardinality match {
      case Multiple =>
        if (isCompositor) selector + ".flatten"
        else selector
      case Optional =>
        if (isCompositor) selector + " getOrElse { Nil}"
        else selector + ".toList"
      case Single =>
        if (isCompositor) selector
        else "Seq(" + selector + ")"
    }
    
    log("Args#buildArgForMixed: " + cardinality + ": " + particle + ": " + retval)
    retval
  }
  
  def buildArgForOptTextRecord(pos: Int): String =
    buildSelector(pos) + ".toList"
    
  def buildArg(typeSymbol: BuiltInSimpleTypeSymbol, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean,
      list: Boolean = false): String = {
        
    val (pre, post) = typeSymbol.name match {
      case "String"     => ("", "")
      case "javax.xml.datatype.Duration" => ("scalaxb.Helper.toDuration(", ")")
      case "javax.xml.datatype.XMLGregorianCalendar" => ("scalaxb.XMLCalendar(", ")")
      case "Boolean"    => ("", ".toBoolean")
      case "Int"        => ("", ".toInt")
      case "Long"       => ("", ".toLong")
      case "Short"      => ("", ".toShort")
      case "Float"      => ("", ".toFloat")
      case "Double"     => ("", ".toDouble")
      case "Byte"       => ("", ".toByte")
      case "BigInt"     => ("BigInt(", ")")
      case "BigDecimal" => ("BigDecimal(", ")")
      case "java.net.URI" => ("scalaxb.Helper.toURI(", ")")
      case "javax.xml.namespace.QName"
        => ("javax.xml.namespace.QName.valueOf(", ")")
      case "Array[String]" => ("scalaxb.Helper.splitBySpace(", ")")
      case "Array[Byte]" => ("scalaxb.Helper.toByteArray(", ")")
      case "scalaxb.HexBinary"  => ("scalaxb.Helper.toHexBinary(", ")") 
      case _        => error("GenSource#buildArg: Unsupported type " + typeSymbol.toString) 
    }
          
    def buildSplitter(r: String) = "scalaxb.Helper.splitBySpace(" + r  + ".text).map(x => " + pre + "x" + post + ")" 
    val retval = (list, cardinality, nillable, defaultValue, fixedValue) match {
      case (true, Multiple, true, _, _) =>
        selector + ".toSeq map { x => if (x.nil) None else Some(" + buildSplitter("x") + ".toSeq) }"
      case (true, Multiple, false, _, _) =>
        selector + ".toSeq map { x => " + buildSplitter("x") + ".toSeq }" 
      case (true, Optional, true, _, _) =>
        selector + ".headOption map { x => if (x.nil) None else Some(" + buildSplitter("x") + ".toSeq) }"
      case (true, Optional, false, _, _) =>
        selector + ".headOption map { x => " + buildSplitter("x") + ".toSeq }"
      case (true, Single, true, _, _) =>
        "if (" + selector + ".nil) None else Some(" + buildSplitter(selector) + ".toSeq)"
      case (true, Single, false, _, _) =>  
        buildSplitter(selector)
      case (false, Multiple, true, _, _) =>
        selector + ".toSeq map { x => if (x.nil) None else Some(" + pre + "x.text" + post + ") }"
      case (false, Multiple, false, _, _) =>
        if (selector.contains("split(")) selector + ".toSeq map { x => " + pre + "x" + post + " }"
        else selector + ".toSeq map { x => " + pre + "x.text" + post + " }"
      case (false, Optional, true, _, _) =>
        selector + ".headOption map { x => if (x.nil) None else Some(" + pre + "x.text" + post + ") }"
      case (false, Optional, false, _, _) =>
        selector + ".headOption map { x => " + pre + "x.text" + post + " }"
      case (false, Single, _, _, Some(x)) =>
        pre + quote(x) + post
      case (false, Single, _, Some(x), _) =>
        selector + ".headOption map { x => " + pre + "x.text" + post + " } getOrElse { " + pre + quote(x) + post + " }"
      case (false, Single, true, _, _) =>
        "if (" + selector + ".nil) None else Some(" + pre + selector + ".text" + post + ")"
      case (false, Single, false, _, _) =>
        buildFromXML(buildTypeName(typeSymbol), selector + ".node")
    }
    
    retval
  }
  
  def buildAttributeGroupArg(group: AttributeGroupDecl): String = {
    val formatterName = buildTypeName(group) + "Format"
    formatterName + ".fromXML(node)"
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
}
