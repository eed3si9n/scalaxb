import org.junit.Test
import org.junit.Assert._
import scala.xml.TopScope
import scalaxb._
import generated._

class ScalaxbTest {

  @Test
  def test() {
    val address = Address("Foo", "1537 Paper Street", "Wilmington")
    val xml = toXML[Address](address, None, Some("address"), TopScope).head

    assertEquals("address", xml.label)
    assertEquals("Foo", xml \ "name" text)
    assertEquals("1537 Paper Street", xml \ "street" text)
    assertEquals("Wilmington", xml \ "city" text)
  }

}
