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
      """case class SingularBuiltInTypeTestSequence(string: String, boolean: Boolean, decimal: BigDecimal, float: Float, double: Double, duration: javax.xml.datatype.Duration, dateTime: javax.xml.datatype.XMLGregorianCalendar, time: javax.xml.datatype.XMLGregorianCalendar, date: javax.xml.datatype.XMLGregorianCalendar, gYearMonth: javax.xml.datatype.XMLGregorianCalendar)""")
  }

  val anyTypeEntitySource = module.processNode(anyTypeXML, "example")(0)

  def anytype1 = {
    println(anyTypeEntitySource)
    anyTypeEntitySource.lines.toList must contain(
      """case class AnyTypeTest(any1: scalaxb.DataRecord[Any], any2: scalaxb.DataRecord[Option[Any]], any3: Option[scalaxb.DataRecord[Any]], any4: Option[scalaxb.DataRecord[Option[Any]]], any5: Seq[scalaxb.DataRecord[Any]], any6: Seq[scalaxb.DataRecord[Option[Any]]])""")
      // """case class AnyTypeTest(any1: scalaxb.DataRecord[Any], any2: scalaxb.DataRecord[Option[Any]], any3: Option[scalaxb.DataRecord[Any]] = None, any4: Option[scalaxb.DataRecord[Option[Any]]] = None, any5: Seq[scalaxb.DataRecord[Any]] = Nil, any6: Seq[scalaxb.DataRecord[Option[Any]]] = Nil)"""
  }
}
