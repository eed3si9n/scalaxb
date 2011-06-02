import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.WordSpec
import org.scalatest.matchers.{MustMatchers,ShouldMatchers}
import scala.xml.Utility.trim
import scalaxb._

@RunWith(classOf[JUnitRunner])
class ScalaxbExampleSpec extends WordSpec with ShouldMatchers with MustMatchers {

  "The XML in the example" should {

    "be parsed correctly" in {
      scalaxb.fromXML[ipo.Address](example.subject) must be === example.address
    }

  }

  "The Address in the example" should {
    "be serialized into XML correctly" in {
      val shipTo = example.address
      val document = scalaxb.toXML[ipo.Address](shipTo.copy(name = "Bar"), None, Some("foo"), ipo.defaultScope)
      document must be === trim(<foo xmlns="http://www.example.com/IPO"
                                         xmlns:ipo="http://www.example.com/IPO"
                                         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <name>Bar</name>
        <street>1537 Paper Street</street>
        <city>Wilmington</city>
      </foo>)
    }
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
