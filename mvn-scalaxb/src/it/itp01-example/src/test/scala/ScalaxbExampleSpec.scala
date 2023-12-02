import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funsuite.AnyFunSuite
import scala.xml.Utility.trim
import scalaxb._

@RunWith(classOf[JUnitRunner])
class ScalaxbExampleSpec extends AnyFunSuite {

  test("The XML in the example should be parsed correctly") {
    assert(scalaxb.fromXML[ipo.Address](example.subject) == example.address)
  }

  test("The Address in the example should be serialized into XML correctly") {
    val shipTo = example.address
    val document = scalaxb.toXML[ipo.Address](shipTo.copy(name = "Bar"), None, Some("foo"), ipo.defaultScope)
    assert(document == trim(
      <foo xmlns="http://www.example.com/IPO"
           xmlns:xs="http://www.w3.org/2001/XMLSchema"
           xmlns:ipo="http://www.example.com/IPO"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <name>Bar</name>
        <street>1537 Paper Street</street>
        <city>Wilmington</city>
      </foo>))
  }

  object example {
    val subject = <shipTo xmlns="http://www.example.com/IPO">
      <name>Foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
    </shipTo>

    val address = ipo.Address("Foo", "1537 Paper Street", "Wilmington")
  }
}
