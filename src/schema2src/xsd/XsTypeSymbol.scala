/**
 * @author  e.e d3si9n
 */
 
package schema2src.xsd

abstract class XsTypeSymbol(val name: String) extends xml.TypeSymbol {
  override def toString(): String = {
    name 
  }  
}

object xsdAny extends XsTypeSymbol("any") {}

class ReferenceTypeSymbol(name: String) extends XsTypeSymbol(name) {
  var decl: TypeDecl = null
}

class SimpleTypeSymbol(name: String) extends XsTypeSymbol(name) {
  var decl: SimpleTypeDecl = null
}

class BuiltInSimpleTypeSymbol(name: String) extends SimpleTypeSymbol(name)

class ComplexTypeSymbol(name: String) extends XsTypeSymbol(name) {
  var decl: ComplexTypeDecl = null
}

abstract class DerivSym
case class Extends(sym: XsTypeSymbol) extends DerivSym
case class Restricts(sym: XsTypeSymbol) extends DerivSym

object xsUnknown          extends BuiltInSimpleTypeSymbol("String") {}
object xsDuration         extends BuiltInSimpleTypeSymbol("javax.xml.datatype.Duration") {}
object xsDateTime         extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsTime             extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsDate             extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsGYearMonth       extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsGYear            extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsGDay             extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsGMonth           extends BuiltInSimpleTypeSymbol("java.util.Calendar") {}
object xsBoolean          extends BuiltInSimpleTypeSymbol("Boolean") {}
object xsFloat            extends BuiltInSimpleTypeSymbol("Float") {}
object xsBase64Binary     extends BuiltInSimpleTypeSymbol("Base64Binary") {}
object xsHexBinary        extends BuiltInSimpleTypeSymbol("HexBinary") {}
object xsDouble           extends BuiltInSimpleTypeSymbol("Double") {}
object xsAnyURI           extends BuiltInSimpleTypeSymbol("java.net.URI") {}
object xsQName            extends BuiltInSimpleTypeSymbol("javax.xml.namespace.QName") {}
object xsNOTATION         extends BuiltInSimpleTypeSymbol("javax.xml.namespace.QName") {}
object xsString           extends BuiltInSimpleTypeSymbol("String") {}
object xsNormalizedString extends BuiltInSimpleTypeSymbol("String") {}
object xsToken            extends BuiltInSimpleTypeSymbol("String") {}
object xsLanguage         extends BuiltInSimpleTypeSymbol("String") {}
object xsName             extends BuiltInSimpleTypeSymbol("String") {}
object xsNMTOKEN          extends BuiltInSimpleTypeSymbol("String") {}
object xsNMTOKENS         extends BuiltInSimpleTypeSymbol("Array[String]") {}
object xsNCName           extends BuiltInSimpleTypeSymbol("String") {}
object xsID               extends BuiltInSimpleTypeSymbol("String") {}
object xsIDREF            extends BuiltInSimpleTypeSymbol("String") {}
object xsIDREFS           extends BuiltInSimpleTypeSymbol("Array[String]") {}
object xsENTITY           extends BuiltInSimpleTypeSymbol("String") {}
object xsENTITIES         extends BuiltInSimpleTypeSymbol("Array[String]") {}
object xsDecimal          extends BuiltInSimpleTypeSymbol("BigDecimal") {}
object xsInteger          extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object xsNonPositiveInteger extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object xsNegativeInteger    extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object xsNonNegativeInteger extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object xsPositiveInteger    extends BuiltInSimpleTypeSymbol("Int") {} // BigInt
object xsLong             extends BuiltInSimpleTypeSymbol("Long") {}
object xsUnsignedLong     extends BuiltInSimpleTypeSymbol("BigInt") {}
object xsInt              extends BuiltInSimpleTypeSymbol("Int") {}
object xsUnsignedInt      extends BuiltInSimpleTypeSymbol("Long") {}
object xsShort            extends BuiltInSimpleTypeSymbol("Short") {}
object xsUnsignedShort    extends BuiltInSimpleTypeSymbol("Int") {}
object xsByte             extends BuiltInSimpleTypeSymbol("Byte") {}
object xsUnsignedByte     extends BuiltInSimpleTypeSymbol("Int") {}

object XsTypeSymbol {
  def toTypeSymbol(value: String) = value match {
    case "duration"       => xsDuration
    case "dateTime"       => xsDateTime
    case "time"           => xsTime
    case "date"           => xsDate
    case "gYearMonth"     => xsGYearMonth
    case "gYear"          => xsGYear
    case "gDay"           => xsGDay
    case "gMonth"         => xsGMonth
    case "boolean"        => xsBoolean
    case "float"          => xsFloat
    case "base64Binary"   => xsBase64Binary
    case "hexBinary"      => xsHexBinary
    case "double"         => xsDouble
    case "anyURI"         => xsAnyURI
    case "QName"          => xsQName
    case "NOTATION"       => xsNOTATION
    case "string"         => xsString
    case "normalizedString" => xsNormalizedString
    case "token"          => xsToken
    case "language"       => xsLanguage
    case "name"           => xsName
    case "NMTOKEN"        => xsNMTOKEN
    case "NMTOKENS"       => xsNMTOKENS
    case "NCName"         => xsNCName
    case "ID"             => xsID
    case "IDREF"          => xsIDREF
    case "IDREFS"         => xsIDREFS
    case "ENTITY"         => xsENTITY
    case "ENTITIES"       => xsENTITIES
    case "decimal"        => xsDecimal
    case "integer"        => xsInteger
    case "nonPositiveInteger" => xsNonPositiveInteger
    case "negativeInteger" => xsNegativeInteger
    case "nonNegativeInteger" => xsNonNegativeInteger
    case "positiveInteger" => xsPositiveInteger
    case "long"           => xsLong
    case "unsignedLong"   => xsUnsignedLong
    case "int"            => xsInt
    case "unsignedInt"    => xsUnsignedInt
    case "short"          => xsShort
    case "unsignedShort"  => xsUnsignedShort
    case "byte"           => xsByte
    case "unsignedByte"   => xsUnsignedByte
    case _                => xsUnknown  
  }  
}
