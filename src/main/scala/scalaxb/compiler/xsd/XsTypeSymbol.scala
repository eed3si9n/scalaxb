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

trait XsTypeSymbol extends scala.xml.TypeSymbol {
  val name: String
  
  override def toString(): String = name 
}

object XsAny extends XsTypeSymbol {
  val name = "scala.xml.Node"
}

object XsAnySimpleType extends XsTypeSymbol {
  val name = "scala.xml.Node"
}

object XsInterNamespace extends XsTypeSymbol {
  val name = "XsInterNamespace"
}

object XsAnyAttribute extends XsTypeSymbol {
  val name = "XsAnyAttribute"
}

object XsMixed extends XsTypeSymbol {
  val name = "XsMixed"
}

case class XsDataRecord(member: XsTypeSymbol) extends XsTypeSymbol {
  val name = "XsDataRecord(" + member + ")"
}

class ReferenceTypeSymbol(val name: String) extends XsTypeSymbol {
  if (name == "") error("ReferenceTypeSymbol#: name cannot be blank.");
  
  var decl: TypeDecl = null
  override def toString(): String = {
    if (decl == null) "ReferenceTypeSymbol(" + name + ",null)"
    else "ReferenceTypeSymbol(" + name + ")"
  }
}

object ReferenceTypeSymbol {
  def unapply(value: ReferenceTypeSymbol): Option[TypeDecl] = Some(value.decl)
}

case class XsXMLFormat(member: Decl) extends XsTypeSymbol {
  val name = "XsXMLFormat(" + (member match {
    case decl: ComplexTypeDecl => decl.name
    case group: AttributeGroupDecl => group.name
    case _ => "_"
  }) + ")"
}

class BuiltInSimpleTypeSymbol(val name: String) extends XsTypeSymbol

case class AttributeGroupSymbol(namespace: Option[String],
  name: String) extends XsTypeSymbol

abstract class DerivSym
case class Extends(sym: XsTypeSymbol) extends DerivSym
case class Restricts(sym: XsTypeSymbol) extends DerivSym

object XsUnknown          extends BuiltInSimpleTypeSymbol("String") {}
object XsDuration         extends BuiltInSimpleTypeSymbol("javax.xml.datatype.Duration") {}
object XsDateTime         extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsTime             extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsDate             extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsGYearMonth       extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsGYear            extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsGDay             extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsGMonth           extends BuiltInSimpleTypeSymbol("javax.xml.datatype.XMLGregorianCalendar") {}
object XsBoolean          extends BuiltInSimpleTypeSymbol("Boolean") {}
object XsFloat            extends BuiltInSimpleTypeSymbol("Float") {}
object XsBase64Binary     extends BuiltInSimpleTypeSymbol("Array[Byte]") {}
object XsHexBinary        extends BuiltInSimpleTypeSymbol("scalaxb.HexBinary") {}
object XsDouble           extends BuiltInSimpleTypeSymbol("Double") {}
object XsAnyURI           extends BuiltInSimpleTypeSymbol("java.net.URI") {}
object XsQName            extends BuiltInSimpleTypeSymbol("javax.xml.namespace.QName") {}
object XsNOTATION         extends BuiltInSimpleTypeSymbol("javax.xml.namespace.QName") {}
object XsString           extends BuiltInSimpleTypeSymbol("String") {}
object XsNormalizedString extends BuiltInSimpleTypeSymbol("String") {}
object XsToken            extends BuiltInSimpleTypeSymbol("String") {}
object XsLanguage         extends BuiltInSimpleTypeSymbol("String") {}
object XsName             extends BuiltInSimpleTypeSymbol("String") {}
object XsNMTOKEN          extends BuiltInSimpleTypeSymbol("String") {}
object XsNMTOKENS         extends BuiltInSimpleTypeSymbol("Array[String]") {}
object XsNCName           extends BuiltInSimpleTypeSymbol("String") {}
object XsID               extends BuiltInSimpleTypeSymbol("String") {}
object XsIDREF            extends BuiltInSimpleTypeSymbol("String") {}
object XsIDREFS           extends BuiltInSimpleTypeSymbol("Array[String]") {}
object XsENTITY           extends BuiltInSimpleTypeSymbol("String") {}
object XsENTITIES         extends BuiltInSimpleTypeSymbol("Array[String]") {}
object XsDecimal          extends BuiltInSimpleTypeSymbol("BigDecimal") {}
object XsInteger          extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object XsNonPositiveInteger extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object XsNegativeInteger    extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object XsNonNegativeInteger extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object XsPositiveInteger    extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object XsLong             extends BuiltInSimpleTypeSymbol("Long") {}
object XsUnsignedLong     extends BuiltInSimpleTypeSymbol("BigInt") {}
object XsInt              extends BuiltInSimpleTypeSymbol("Int") {}
object XsUnsignedInt      extends BuiltInSimpleTypeSymbol("Long") {}
object XsShort            extends BuiltInSimpleTypeSymbol("Short") {}
object XsUnsignedShort    extends BuiltInSimpleTypeSymbol("Int") {}
object XsByte             extends BuiltInSimpleTypeSymbol("Byte") {}
object XsUnsignedByte     extends BuiltInSimpleTypeSymbol("Int") {}

object XsTypeSymbol {
  type =>?[A, B] = PartialFunction[A, B]
  
  val toTypeSymbol: String =>? XsTypeSymbol = {
    case "anyType"        => XsAny
    case "anySimpleType"  => XsAnySimpleType
    case "duration"       => XsDuration
    case "dateTime"       => XsDateTime
    case "time"           => XsTime
    case "date"           => XsDate
    case "gYearMonth"     => XsGYearMonth
    case "gYear"          => XsGYear
    case "gDay"           => XsGDay
    case "gMonth"         => XsGMonth
    case "boolean"        => XsBoolean
    case "float"          => XsFloat
    case "base64Binary"   => XsBase64Binary
    case "hexBinary"      => XsHexBinary
    case "double"         => XsDouble
    case "anyURI"         => XsAnyURI
    case "QName"          => XsQName
    case "NOTATION"       => XsNOTATION
    case "string"         => XsString
    case "normalizedString" => XsNormalizedString
    case "token"          => XsToken
    case "language"       => XsLanguage
    case "name"           => XsName
    case "NMTOKEN"        => XsNMTOKEN
    case "NMTOKENS"       => XsNMTOKENS
    case "NCName"         => XsNCName
    case "ID"             => XsID
    case "IDREF"          => XsIDREF
    case "IDREFS"         => XsIDREFS
    case "ENTITY"         => XsENTITY
    case "ENTITIES"       => XsENTITIES
    case "decimal"        => XsDecimal
    case "integer"        => XsInteger
    case "nonPositiveInteger" => XsNonPositiveInteger
    case "negativeInteger" => XsNegativeInteger
    case "nonNegativeInteger" => XsNonNegativeInteger
    case "positiveInteger" => XsPositiveInteger
    case "long"           => XsLong
    case "unsignedLong"   => XsUnsignedLong
    case "int"            => XsInt
    case "unsignedInt"    => XsUnsignedInt
    case "short"          => XsShort
    case "unsignedShort"  => XsUnsignedShort
    case "byte"           => XsByte
    case "unsignedByte"   => XsUnsignedByte 
  }  
}
