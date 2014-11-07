/**
 * @author  e.e d3si9n
 */

import scalaxb._
import mixed._

object MixedUsage {
  def main(args: Array[String]) = {
    allTests
  }

  def allTests = {
    testMixed
    true
  }
  
  def testMixed {
    val subject = <foo xmlns="http://www.example.com/mixed"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">foo<name>Jane</name><name>Doe</name><city>New York</city>bar
          <billTo>
            <name>Foo</name>
            <street>1537 Paper Street</street>
            <city>Wilmington</city>
          </billTo>
          <itemname/><itemcode/><option1/></foo>
    val obj = fromXML[MixedTest](subject)
    obj match {
      case MixedTest(Seq(
          DataRecord(None, None, "foo"),
          DataRecord(Some("http://www.example.com/mixed"), Some("name"), "Jane"),
          DataRecord(Some("http://www.example.com/mixed"), Some("name"), "Doe"),
          DataRecord(Some("http://www.example.com/mixed"), Some("city"), "New York"),
          DataRecord(None, None, _),
          DataRecord(Some("http://www.example.com/mixed"), Some("billTo"), _),
          DataRecord(None, None, _),
          DataRecord(Some("http://www.example.com/mixed"), Some("itemname"), ""),
          DataRecord(Some("http://www.example.com/mixed"), Some("itemcode"), ""),
          DataRecord(Some("http://www.example.com/mixed"), Some("option1"), "")
        ), _) =>
      case _ => sys.error("match failed: " + obj.toString)
    }
    val document = toXML[MixedTest](obj, "foo", defaultScope)
    println(document)
  }
}
