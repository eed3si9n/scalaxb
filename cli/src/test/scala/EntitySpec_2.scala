package scalaxb.specs

import org.specs2._

object EntitySpec_2 extends Specification { def is =                          s2"""
  xs:string should
    be referenced as String                                                   $builtin1
  
  xs:anyType should
    be referenced as DataRecord[Any] if it's non-nillable                     $anytype1
    be referenced as DataRecord[Option[Any]] if it's nillable                 $anytype1"""

  import Example._
  // scalaxb.compiler.Module.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver
  
  val builtInEntitySource = module.processNode(builtInTypesXML, "example")(0)

  def builtin1 = {
    println(builtInEntitySource)
    builtInEntitySource.lines.toList must contain(
      """case class SingularBuiltInTypeTest(string: String, boolean: Boolean, decimal: BigDecimal, float: Float, double: Double, duration: javax.xml.datatype.Duration, dateTime: javax.xml.datatype.XMLGregorianCalendar, time: javax.xml.datatype.XMLGregorianCalendar, date: javax.xml.datatype.XMLGregorianCalendar, gYearMonth: javax.xml.datatype.XMLGregorianCalendar, gYear: javax.xml.datatype.XMLGregorianCalendar, gMonthDay: javax.xml.datatype.XMLGregorianCalendar, gDay: javax.xml.datatype.XMLGregorianCalendar, gMonth: javax.xml.datatype.XMLGregorianCalendar, hexBinary: scalaxb.HexBinary, base64Binary: scalaxb.Base64Binary, anyURI: java.net.URI, QName: javax.xml.namespace.QName, NOTATION: javax.xml.namespace.QName, normalizedString: String, token: String, language: String, NMTOKEN: String, NMTOKENS: Seq[String], Name: String, NCName: String, ID: String, IDREF: String, IDREFS: Seq[String], ENTITY: String, ENTITIES: Seq[String], integer: BigInt, nonPositiveInteger: BigInt, negativeInteger: BigInt, long: Long, int: Int, short: Short, byte: Byte, nonNegativeInteger: BigInt, unsignedLong: BigInt, unsignedInt: Long, unsignedShort: Int, unsignedByte: Int, positiveInteger: BigInt, anyType: scalaxb.DataRecord[Any], anySimpleType: scalaxb.DataRecord[Any])""")
  }

  val anyTypeEntitySource = module.processNode(anyTypeXML, "example")(0)

  def anytype1 = {
    println(anyTypeEntitySource)
    anyTypeEntitySource.lines.toList must contain(
      """case class AnyTypeTest(any1: scalaxb.DataRecord[Any], any2: scalaxb.DataRecord[Option[Any]], any3: Option[scalaxb.DataRecord[Any]], any4: Option[scalaxb.DataRecord[Option[Any]]], any5: Seq[scalaxb.DataRecord[Any]], any6: Seq[scalaxb.DataRecord[Option[Any]]])""")
      // """case class AnyTypeTest(any1: scalaxb.DataRecord[Any], any2: scalaxb.DataRecord[Option[Any]], any3: Option[scalaxb.DataRecord[Any]] = None, any4: Option[scalaxb.DataRecord[Option[Any]]] = None, any5: Seq[scalaxb.DataRecord[Any]] = Nil, any6: Seq[scalaxb.DataRecord[Option[Any]]] = Nil)"""
  }
}
