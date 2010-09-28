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
 
trait Args extends Params {
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl        => buildArg(elem, 0)
    case attr: AttributeDecl   => buildArg(attr)
    case ref: AttributeRef     => buildArg(buildAttribute(ref))
    case group: AttributeGroupDecl => buildArg(group)
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
      if (containsEnumeration(decl)) buildArgForComplexType(buildTypeName(decl) ,selector,
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
      
      if (elem.maxOccurs > 1) selector + ".toList"
      else selector
    }
    else buildArgForComplexType(buildTypeName(elem.typeSymbol), selector,
      elem.defaultValue, elem.fixedValue,
      toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), false)
  
  def buildArgForComplexType(typeName: String, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean,
      list: Boolean): String = {    
    val optionSelector = if (selector contains("@")) selector + ".headOption"
      else selector
    val splitter = ".text.split(' ').toList.map { x => " + typeName + ".fromString(x) }" 
    
    val retval = (list, cardinality, nillable) match {
      case (true, Multiple, true) =>
        selector + ".toList.map(x => if (x.nil) None" + newline +
          indent(3) + "else Some(x" + splitter + ") )"
      case (true, Multiple, false) =>
        selector + ".toList.map(x => x" + splitter + ")"
      case (true, Optional, true) =>
        optionSelector + " match {" + newline +
        indent(4) + "case Some(x) => if (x.nil) None" + newline +
        indent(4) + "  else Some(x" + splitter + ")" + newline +
        indent(4) + "case None    => None" + newline +
        indent(3) + "}"
      case (true, Optional, false) =>
        selector + ".headOption map { x => x" + splitter + " }"
      case (true, Single, true) =>
        "if (" + selector + ".nil) None" + newline +
          indent(3) + "else Some(" + selector + splitter + ")"
      case (true, Single, false) =>
        selector + splitter
      case (false, Multiple, true) =>
        selector + ".toList.map(x => if (x.nil) None" + newline +
          indent(3) + "else Some(" + typeName + ".fromXML(x.node)) )"        
      case (false, Multiple, false) =>
        selector + ".toList.map(x => " + typeName + ".fromXML(x))"
      case (false, Optional, true) =>
        optionSelector + " match {" + newline +
          indent(4) + "case Some(x) => if (x.nil) None" + newline +
          indent(4) + "  else Some(" +  typeName + ".fromXML(x.node))" + newline +
          indent(4) + "case None    => None" + newline +
          indent(3) + "}"
      case (false, Optional, false) =>
        selector + ".headOption map { x => " + typeName + ".fromXML(x) }"
      case (false, Single, true) =>
        "if (" + selector + ".nil) None" + newline +
          indent(3) + "else Some(" +  typeName + ".fromXML(" + selector + ".node))"
      case (false, Single, false) =>
        fixedValue match {
          case Some(x) => 
            typeName + ".fromString(" + quote(x) + ")"
          case None => 
            defaultValue match {
              case Some(y) =>
                optionSelector + " match {" + newline +
                indent(4) + "case Some(x) => " + typeName + ".fromXML(x)" + newline +
                indent(4) + "case None    => " + typeName + ".fromString(" + quote(y) + ")" + newline +
                indent(3) + "}"
              case None => typeName + ".fromXML(" + selector + ")" 
            }
        }      
    }
    
    log("GenSource#buildArgForComplexType: " + typeName + ": " + retval)
    retval
  }
  
  def buildArgForAny(selector: String, namespace: Option[String], elementLabel: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean) = {
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      if (nillable) selector + " match {" + newline +
          indent(4) + "case Some(x) => if (x.nil) " + noneValue + newline +
          indent(4) + "  else " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}"
      else selector + " match {" + newline +
        indent(4) + "case Some(x) => " + someValue + newline +
        indent(4) + "case None    => " + noneValue + newline +
        indent(3) + "}"    
    
    def hardcoded(value: String) =
      "scala.xml.Elem(node.scope.getPrefix(" + quote(schema.targetNamespace) + "), "  + newline +
      indent(4) + quote(elementLabel) + ", scala.xml.Null, node.scope, " +  newline +
      indent(4) + "scala.xml.Text(" + quote(value) + "))"
    
    val retval = (cardinality, nillable, defaultValue, fixedValue) match {
      case (Multiple, true, _, _) =>
        selector + ".toList.map { x => if (x.nil) None" + newline +
          indent(3) + "else Some(x.toDataRecord) }"
      case (Multiple, false, _, _) =>
        selector + ".toList.map { _.toDataRecord }"
      case (Optional, _, _, _) =>
        buildMatchStatement("None", "Some(x.toDataRecord)")
      case (Single, _, _, Some(x)) =>
        "rt.DataRecord(" + newline +
          indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
          indent(4) + hardcoded(x) + "," + newline +
          indent(4) + "None)"
      case (Single, _, Some(x), _) =>
        buildMatchStatement("rt.DataRecord(" + newline +
          indent(4) + quote(namespace) + ", " + quote(elementLabel) + ", " + newline +
          indent(4) + hardcoded(x) + "," + newline +
          indent(4) + "None)",
          "x.toDataRecord")        
      case (Single, false, _, _) =>
        selector + ".toDataRecord"
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
      if (containsEnumeration(decl)) buildArgForComplexType(buildTypeName(decl) ,selector,
        defaultValue, fixedValue,
        cardinality, nillable, false)
      else buildArg(baseType(decl), selector, defaultValue, fixedValue, cardinality, nillable)
    case SimpTypListDecl(ReferenceTypeSymbol(itemType: SimpleTypeDecl)) 
          if containsEnumeration(itemType) =>
      buildArgForComplexType(buildTypeName(itemType) ,selector,
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
    indent(4) + "      List(rt.DataRecord(None, Some(key), value.text, None))" + newline +
    indent(4) + "    case scala.xml.PrefixedAttribute(pre, key, value, _) =>" + newline +
    indent(4) + "      List(rt.DataRecord(Option[String](elem.scope.getURI(pre)), Some(key), value.text, None))" + newline +
    indent(4) + "    case _ => Nil" + newline +
    indent(4) + "  }" + newline +
    indent(4) + "case _ => Nil" + newline +
    indent(3) + "}" 
  }
  
  // def buildArgForMixed(particles: List[Decl]): String = {
  //   "(node.child.map {" + newline +
  //   indent(3) + fromXmlCases(particles, 3).mkString(newline + indent(3)) + newline +
  //   indent(3) + "case x: scala.xml.Text =>" + newline +
  //   indent(3) + "  rt.DataRecord(None, None, x.text)" + newline +
  //   indent(2) + "}).toList"
  // }
  
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
  
  def buildArg(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAny => selector
    case symbol: BuiltInSimpleTypeSymbol =>
      buildArg(symbol, selector, None, None, Single, false)
    case ReferenceTypeSymbol(decl@SimpleTypeDecl(_, _, _, _, _)) =>
      buildArg(baseType(decl), selector, None, None, Single, false)
    case _ =>
      buildTypeName(typeSymbol) + ".fromXML(" + selector + ")"
  }
  
  def buildWriter(selector: String, typeSymbol: XsTypeSymbol): String =
    "None"
  
  def fromXmlCases(particles: List[Decl], indentBase: Int) = {
    def makeCaseEntry(elem: ElemDecl) =
      "case x: scala.xml.Elem if (x.label == " + quote(elem.name) + " && " + newline + 
        indent(indentBase + 2) + "Option[String](x.scope.getURI(x.prefix)) == " + quote(elem.namespace) + ") =>" + newline +
        indent(indentBase + 1) + "rt.DataRecord(Option[String](x.scope.getURI(x.prefix)), Some(x.label), " +
        buildArg("x", elem.typeSymbol) + ", " + buildWriter("x", elem.typeSymbol) + ")"
    
    def isAnyOrChoice(typeSymbol: XsTypeSymbol) = typeSymbol match {
      case XsAny => true
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) if compositorWrapper.contains(decl) =>
        true
      case _ => false
    }
    
    def makeListForAny = if (particles.exists(x => x.isInstanceOf[AnyDecl] ||
        (x.isInstanceOf[ElemDecl] && (isAnyOrChoice(x.asInstanceOf[ElemDecl].typeSymbol))) ||
        (x.isInstanceOf[ElemRef] && (isAnyOrChoice(buildElement(x.asInstanceOf[ElemRef]).typeSymbol)))  ))
      List("case x: scala.xml.Elem =>" + newline +
        indent(indentBase + 1) + "rt.DataRecord(Option[String](x.scope.getURI(x.prefix)), Some(x.label), x, None)")
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
      cardinality: Cardinality, nillable: Boolean,
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
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      if (nillable) optionSelector + " match {" + newline +
          indent(4) + "case Some(x) => if (x.nil) " + noneValue + newline +
          indent(4) + "  else " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}" 
      else optionSelector + " match {" + newline +
          indent(4) + "case Some(x) => " + someValue + newline +
          indent(4) + "case None    => " + noneValue + newline +
          indent(3) + "}"
    
    def buildMapStatement(someValue: String) =
       selector + ".headOption map { x => " + someValue + " }"
    
    val splitter = ".text.split(' ').toList.map { x => " + pre + "x" + post + " }" 
    val retval = (list, cardinality, nillable, defaultValue, fixedValue) match {
      case (true, Multiple, true, _, _) =>
        selector + ".toList.map(x => if (x.nil) None" + newline +
          indent(3) + "else Some(x" + splitter + ") )"
      case (true, Multiple, false, _, _) =>
        selector + ".toList.map(x => x" + splitter + ")" 
      case (true, Optional, true, _, _) =>
        buildMatchStatement("None", "Some(x" + splitter + ")")
      case (true, Optional, false, _, _) =>
        buildMapStatement("x" + splitter)
      case (true, Single, true, _, _) =>
        "if (" + selector + ".nil) None" + newline +
          indent(3) + "else Some(" + selector + splitter + ")"
      case (true, Single, false, _, _) =>  
        selector + splitter
      case (false, Multiple, true, _, _) =>
        selector + ".toList.map(x => if (x.nil) None" + newline +
          indent(3) + "else Some(" + pre + "x.text" + post + ") )"
      case (false, Multiple, false, _, _) =>
        if (selector.contains("split(")) selector + ".toList.map { x => " + pre + "x" + post + " }"
        else selector + ".toList.map { x => " + pre + "x.text" + post + " }"
      case (false, Optional, true, _, _) =>
        buildMatchStatement("None", "Some(" + pre + "x.text" + post + ")")
      case (false, Optional, false, _, _) =>
        buildMapStatement(pre + "x.text" + post)
      case (false, Single, _, _, Some(x)) =>
        pre + quote(x) + post
      case (false, Single, _, Some(x), _) =>
        buildMatchStatement(pre + quote(x) + post, pre + "x.text" + post)
      case (false, Single, true, _, _) =>
        "if (" + selector + ".nil) None" + newline +
          indent(3) + "else Some(" + pre + selector + ".text" + post + ")"
      case (false, Single, false, _, _) =>
        pre + selector + ".text" + post
    }
    
    retval
  }
  
  def buildArg(group: AttributeGroupDecl): String = {
    val typeName = buildTypeName(group)
    typeName + ".fromXML(node)"
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
