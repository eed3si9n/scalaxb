package scalaxb.specs

import org.specs2._

object ProtocolSpec_2 extends Specification { def is = sequential             ^
  "this is a specification to check the generated protocol source"            ^
                                                                              p^
  "xs:anyType should"                                                         ^
    "generate combinator parser"                                              ! anytype1^
                                                                              end

  import Example._
  lazy val module = new scalaxb.compiler.xsd2.Driver
  val anyTypeProtocolSource = module.processNode(anyTypeXML, "example")(1)

  def anytype1 = {
    println(anyTypeProtocolSource)
    anyTypeProtocolSource.lines.toList must contain(
      """      scalaxb.fromXML[scalaxb.DataRecord[Option[Any]]](x, scalaxb.ElemName(node) :: stack)""")
    }
}
