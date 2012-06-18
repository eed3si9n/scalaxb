package scalaxb.specs

import org.specs2._

object EntitySpec_2 extends Specification { def is = sequential               ^
  "xs:string should"                                                          ^
    "be referenced as String"                                                 ! builtin1^
                                                                              end^
                                                                              end

  import Example._
  // scalaxb.compiler.Module.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver
  
  val builtInEntitySource = module.processNode(builtInTypesXML, "example")(0)

  def builtin1 = {
    println(builtInEntitySource)
    builtInEntitySource.lines.toList must contain(
      """case class SingularBuiltInTypeTestSequence(string: String, boolean: Boolean, decimal: BigDecimal, float: Float, double: Double, duration: javax.xml.datatype.Duration, dateTime: javax.xml.datatype.XMLGregorianCalendar, time: javax.xml.datatype.XMLGregorianCalendar, date: javax.xml.datatype.XMLGregorianCalendar, gYearMonth: javax.xml.datatype.XMLGregorianCalendar)""")
  }
}
