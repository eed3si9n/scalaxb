package testpkg

import scala.xml.XML
import xhtml.Html
import xhtml.given


object XhtmlTest extends verify.BasicTestSuite:
  lazy val document = XML.loadString("""<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
  <head>
    <title>foo</title>
  </head>
  <body></body>
</html>""")

  test("fromXML") {
    val html = scalaxb.fromXML[Html](document)
    assertEquals(
      html.toString,
      "Html(Head(List(),DataRecord(HeadSequence1(Title(List(DataRecord(foo)),ListMap()),List(),None)),ListMap()),Body(List(),ListMap()),ListMap(@lang -> DataRecord(en)))",
    )
  }

  test("round trip") {
    val html = scalaxb.fromXML[Html](document)
    val roundTrip =
      scalaxb.toXML(html, Some("http://www.w3.org/1999/xhtml"), Some("html"), document.scope)
    assertEquals(
      roundTrip.toString,
      """<html lang="en" xmlns="http://www.w3.org/1999/xhtml"><head><title>foo</title></head><body/></html>""",
    )
  }
end XhtmlTest
