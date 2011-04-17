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
  def buildFromXML(typeName: String): String = "scalaxb.fromXML[" + typeName + "]"
  def buildFromXML(typeName: String, selector: String, stackString: String): String =
    buildFromXML(typeName) + "(%s, %s)".format(selector, stackString)
  
  def buildToXML(typeName: String, args: String): String =
    "scalaxb.toXML[" + typeName + "](" + args + ")"
        
  def buildFromString(typeName: String, selector: String): String =
    typeName + ".fromString(" + selector + ")"
  
  // called by buildConverter
  def buildArg(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAnyType                                  => selector
    case symbol: BuiltInSimpleTypeSymbol            => buildArg(buildTypeName(symbol), selector, Single)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl)  => buildArg(buildTypeName(baseType(decl)), selector, Single)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      buildFromXML(buildTypeName(typeSymbol), selector, "scalaxb.ElemName(node) :: stack")
  }
  
  def buildArg(typeName: String, selector: String, cardinality: Cardinality,
      nillable: Boolean = false, defaultValue: Option[String] = None, fixedValue: Option[String] = None,
      wrapForLongAll: Boolean = false): String = {    
    def fromSelector = buildFromXML(typeName, selector, "scalaxb.ElemName(node) :: stack")
    def fromU = buildFromXML(typeName, "_", "scalaxb.ElemName(node) :: stack")
    def fromValue(x: String) = buildFromXML(typeName, "scala.xml.Text(" + quote(x) + ")", "scalaxb.ElemName(node) :: stack")
    
    val retval = if (wrapForLongAll) {
      // PrefixedAttribute only contains pre, so you need to pass in node to get the namespace.
      if (selector.contains("@")) selector + ".headOption map { x => scalaxb.DataRecord(x, node, " +
        buildFromXML(typeName, "x", "scalaxb.ElemName(node) :: stack") + ") }"
      else selector + ".headOption map { x => scalaxb.DataRecord(x, " +
        buildFromXML(typeName, "x", "scalaxb.ElemName(node) :: stack") + ") }"
    } else (cardinality, nillable) match {
      case (Multiple, true)  => selector + ".toSeq map { _.nilOption map { " + fromU + " }}"
      case (Multiple, false) => selector + ".toSeq map { " + fromU + " }"
      case (Optional, true)  => selector + ".headOption map { _.nilOption map { " + fromU + " }}"
      case (Optional, false) => selector + ".headOption map { " + fromU + " }"
      case (Single, _) =>
        (nillable, defaultValue, fixedValue) match { 
          case ( _, _, Some(x)) => fromValue(x)
          case (_, Some(x), _)  => selector + ".headOption map { " + fromU + " } getOrElse { " + fromValue(x) + " }"
          case (true, _, _)     => selector + ".nilOption map { " + fromU + " }"
          case (false, _, _)    => fromSelector
        }
    }
    
    retval
  }
      
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl        => buildArg(elem, 0)
    case attr: AttributeDecl   => buildArg(attr, buildSelector(attr), false)
    case ref: AttributeRef     => buildArg(buildAttribute(ref))
    case group: AttributeGroupDecl => buildAttributeGroupArg(group, false)
    case _ => error("GenSource#buildArg unsupported delcaration " + decl.toString)
  }
  
  def buildArgForAttribute(decl: AttributeLike, longAttribute: Boolean): String =
    if (longAttribute) {
      decl match {
        case attr: AttributeDecl   => 
          val o = attr.copy(use = OptionalUse)
          val arg = buildArg(o, buildSelector(o), longAttribute)
          if (longAttribute) arg + " map { " + quote(buildNodeName(o, false)) + " -> _ }"
          else arg
        case ref: AttributeRef     => buildArgForAttribute(buildAttribute(ref), longAttribute)
        case group: AttributeGroupDecl => buildAttributeGroupArg(group, longAttribute)
      }
    } else buildArg(decl)
    
  def toOptional(that: ElemDecl) = that.copy(minOccurs = 0, annotation = None)
    
  // called by makeCaseClassWithType. By spec, <all> contains only elements.
  def buildArgForAll(particle: Particle, longAll: Boolean): String = {
    val o = particle match {
      case elem: ElemDecl => toOptional(elem)
      case ref: ElemRef   => toOptional(buildElement(ref))
      case _ => error("buildArgForAll unsupported type: " + particle)
    }
    val arg = buildArg(o, buildSelector(o), longAll)
    if (longAll) arg + " map { " + quote(buildNodeName(o, true)) + " -> _ }"
    else arg
  }
  
  def buildArg(elem: ElemDecl, pos: Int): String =
    buildArg(elem, buildSelector(pos), false)
  
  def buildArg(elem: ElemDecl, selector: String, wrapForLongAll: Boolean): String =
    if ((isSubstitionGroup(elem))) selector
    else elem.typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => buildArg(buildTypeName(symbol), selector, 
        toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), elem.defaultValue, elem.fixedValue, wrapForLongAll)
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
        if (containsEnumeration(decl)) buildArg(buildTypeName(decl, false), selector, 
          toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), elem.defaultValue, elem.fixedValue, wrapForLongAll)
        else buildArg(decl, selector, elem.defaultValue, elem.fixedValue,
          toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), wrapForLongAll) 
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (compositorWrapper.contains(decl))
          (toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse {false}) match {
            case (Multiple, _)    => selector + ".toSeq"
            case (Optional, true) => selector + " getOrElse { None }"
            case _ => selector
          }
        else buildArg(buildTypeName(decl, false), selector,
          toCardinality(elem.minOccurs, elem.maxOccurs), elem.nillable getOrElse(false), elem.defaultValue, elem.fixedValue, wrapForLongAll)
      case AnyType(symbol) => buildArg(
          if (elem.nillable getOrElse(false)) buildTypeName(XsNillableAny)
          else buildTypeName(symbol), selector,
        toCardinality(elem.minOccurs, elem.maxOccurs), false, elem.defaultValue, elem.fixedValue, wrapForLongAll)
      
      case symbol: ReferenceTypeSymbol =>
        if (symbol.decl == null) error("GenSource#buildArg: " + elem.toString + " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with null decl")
        else error("GenSource#buildArg: " + elem.toString + " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with " + symbol.decl.toString)
      case _ => error("GenSource#buildArg: " + elem.toString + " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)    
    }
  
  def buildArg(attr: AttributeDecl, selector: String, wrapForLong: Boolean): String = attr.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildArg(buildTypeName(symbol), selector, 
      toCardinality(attr), false, attr.defaultValue, attr.fixedValue, wrapForLong) 
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildArg(decl, selector, attr.defaultValue, attr.fixedValue,
      toCardinality(attr), false, wrapForLong)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => error("Args: Attribute with complex type " + decl.toString)
    case _ => error("Args: unsupported type: " + attr.typeSymbol)
  }
  
  def buildArg(decl: SimpleTypeDecl, selector: String,
      defaultValue: Option[String], fixedValue: Option[String],
      cardinality: Cardinality, nillable: Boolean, wrapForLongAll: Boolean): String =  
    buildArg(buildTypeName(decl, false), selector, cardinality, nillable, defaultValue, fixedValue, wrapForLongAll)
    
  // called by makeCaseClassWithType
  def buildArg(content: SimpleContentDecl, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case AnyType(symbol) => buildArg(buildTypeName(symbol), "node", Single)
    case base: BuiltInSimpleTypeSymbol => buildArg(buildTypeName(base), "node", Single)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      decl.content match {
        case simp@SimpleContentDecl(SimpContRestrictionDecl(base: XsTypeSymbol, _, _, _)) => buildArg(simp, base)      
        case simp@SimpleContentDecl(SimpContExtensionDecl(base: XsTypeSymbol, _)) => buildArg(simp, base)        
        
        // http://www.w3.org/TR/xmlschema-1/#d0e7923
        case comp: ComplexContentDecl => content match {
          case SimpleContentDecl(SimpContRestrictionDecl(_, Some(simpleType: XsTypeSymbol), _, _))  =>
            buildArg(content, simpleType)
          case _ => error("Args: Unsupported content " + content.toString)
        }
        case _ => error("Args: Unsupported content " + content.toString)
      }
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildArg(decl, "node", None, None, Single, false, false)
        
    case _ => error("Args: Unsupported type " + typeSymbol.toString)    
  }

  // scala's <foo/> \ "foo" syntax is not namespace aware, but {ns}foo is useful for long all.
  def buildNodeName(elem: ElemDecl, prependNamespace: Boolean): String =
    if (elem.global) elem.namespace match {
      case None => elem.name
      case Some(ns) =>
        if (prependNamespace) "{%s}".format(ns) + elem.name
        else elem.name
    }
    else elem.name

  // scala's <foo ns:attr1="" /> \ "@{uri}attr1" syntax requires ns, but {uri} should be ommitted for long attr.
  def buildNodeName(attr: AttributeDecl, prependNamespace: Boolean): String =
    if (attr.global) "@" + (attr.namespace map { "{" + _ + "}" } getOrElse {""}) + attr.name
    else if (prependNamespace && attr.qualified) "@" +
      (elementNamespace(attr.global, attr.namespace, attr.qualified) map { "{" + _ + "}" } getOrElse {""}) + attr.name
    else "@" + attr.name
  
  def buildNodeName(group: AttributeGroupDecl): String = 
    if (group.namespace == schema.targetNamespace) group.name
    else (group.namespace map { "{" + _ + "}" } getOrElse {""}) + group.name
  
  def buildSelector(elem: ElemDecl): String = buildSelector(buildNodeName(elem, false))
  def buildSelector(pos: Int): String = "p" + (pos + 1)
  def buildSelector(attr: AttributeDecl): String = buildSelector(buildNodeName(attr, true))
  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  
  def buildArgForAnyAttribute(parent: ComplexTypeDecl, longAttribute: Boolean): String =
    buildArgForAnyAttribute(flattenAttributes(parent), longAttribute)
  
  def buildArgForAnyAttribute(parent: AttributeGroupDecl, longAttribute: Boolean): String =
    buildArgForAnyAttribute(flattenAttributes(parent.attributes), longAttribute)
  
  def buildArgForAnyAttribute(attributes: List[AttributeLike], longAttribute: Boolean): String = {
    def makeCaseEntry(attr: AttributeDecl) = if (attr.global || attr.qualified)
      "case scala.xml.PrefixedAttribute(pre, key, value, _) if pre == elem.scope.getPrefix(" +
        (attr.namespace map { quote(_) } getOrElse { "null" }) + ") &&" + newline +
      indent(8) + "key == " + quote(attr.name) + " => Nil"
    else "case scala.xml.UnprefixedAttribute(key, value, _) if key == " + quote(attr.name) + " => Nil"
    
    val xs = "node match {" + newline +
    indent(5) + "case elem: scala.xml.Elem =>" + newline +
    indent(5) + "  elem.attributes.toList flatMap {" + newline +
    attributes.collect {
      case x: AttributeDecl => makeCaseEntry(x)
    }.mkString(indent(7), newline + indent(7), newline) +
    indent(5) + "    case scala.xml.UnprefixedAttribute(key, value, _) =>" + newline +
    indent(5) + "      List((\"@\" + key, scalaxb.DataRecord(None, Some(key), value.text)))" + newline +
    indent(5) + "    case scala.xml.PrefixedAttribute(pre, key, value, _) =>" + newline +
    indent(5) + "      val ns = elem.scope.getURI(pre)" + newline +
    indent(5) + "      List((\"@{\" + ns + \"}\" + key, scalaxb.DataRecord(Option[String](ns), Some(key), value.text)))" + newline +
    indent(5) + "    case _ => Nil" + newline +
    indent(5) + "  }" + newline +
    indent(5) + "case _ => Nil" + newline +
    indent(4) + "}"
    
    if (longAttribute) xs
    else "scala.collection.immutable.ListMap((" + xs + "): _*)"
  }
    
  def buildArgForMixed(particle: Particle, pos: Int): String =
    buildArgForMixed(particle, buildSelector(pos))
  
  def buildArgForMixed(particle: Particle, selector: String): String = {
    val cardinality = toCardinality(particle.minOccurs, particle.maxOccurs)
    val isCompositor = particle match {
      case ref: GroupRef => true
      case elem: ElemDecl =>
        elem.typeSymbol match {
          case XsAnyType => true
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
        if (isCompositor) selector + " getOrElse {Nil}"
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
    
  def buildAttributeGroupArg(group: AttributeGroupDecl, longAttribute: Boolean): String = {
    val formatterName = buildFormatterName(group)
    val arg = formatterName + ".reads(node).right"
    if (longAttribute) arg + ".toOption map { x => " + 
      quote(buildNodeName(group)) + " -> scalaxb.DataRecord(None, None, x) }"
    else arg + ".get"
  }

  def flattenAttributes(decl: ComplexTypeDecl): List[AttributeLike] = {
    val attributes = mergeAttributes(decl.content.content match {
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) => flattenAttributes(base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _)         => flattenAttributes(base)
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _)    => flattenAttributes(base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _)      => flattenAttributes(base)
      case _ => Nil
    }, flattenAttributes(decl.content.content.attributes))

    // rearrange attributes so AnyAttributeDecl comes at the end.
    val notAnyAttributes = attributes filter {
      case any: AnyAttributeDecl => false
      case _ => true
    }
    val anyAttributes = attributes filter {
      case any: AnyAttributeDecl => true
      case _ => false
    }
    if (anyAttributes.isEmpty) notAnyAttributes
    else notAnyAttributes ::: List(anyAttributes.head)
  }
  
  // return a list of either AttributeDecl or AnyAttributeDecl
  def flattenAttributes(attributes: List[AttributeLike]): List[AttributeLike] =
    attributes flatMap {
      case any: AnyAttributeDecl => List(any)
      case attr: AttributeDecl => List(attr)
      case ref: AttributeRef   => List(buildAttribute(ref))
      case group: AttributeGroupDecl => flattenAttributes(group.attributes)
      case ref: AttributeGroupRef    => flattenAttributes(buildAttributeGroup(ref).attributes)
    }

  def mergeAttributes(parent: List[AttributeLike],
      child: List[AttributeLike]): List[AttributeLike] = child match {
    case x :: xs => mergeAttributes(mergeAttributes(parent, x), xs)
    case Nil => parent
  }

  def mergeAttributes(parent: List[AttributeLike],
      child: AttributeLike): List[AttributeLike] =
    if (!parent.exists(x => isSameAttribute(x, child))) parent ::: List(child)
    else parent.map (x =>
      if (isSameAttribute(x, child)) child match {
        // since OO's hierarchy does not allow base members to be ommited,
        // child overrides needs to be implemented some other way.
        case attr: AttributeDecl =>
          Some(x)
        case _ => Some(x)
      }
      else Some(x) ).flatten

  def isSameAttribute(lhs: AttributeLike, rhs: AttributeLike) = {
    def resolveRef(attribute: AttributeLike): AttributeLike = attribute match {
      case any: AnyAttributeDecl => any
      case attr: AttributeDecl => attr
      case ref: AttributeRef   => buildAttribute(ref)
      case group: AttributeGroupDecl => group
      case ref: AttributeGroupRef    => buildAttributeGroup(ref)
    }

    (resolveRef(lhs), resolveRef(rhs)) match {
      case (x: AnyAttributeDecl, y: AnyAttributeDecl) => true
      case (x: AttributeDecl, y: AttributeDecl) =>
        (x.name == y.name && x.namespace == y.namespace)
      case (x: AttributeGroupDecl, y: AttributeGroupDecl) =>
        (x.name == y.name && x.namespace == y.namespace)
      case _ => false
    }
  }

}
