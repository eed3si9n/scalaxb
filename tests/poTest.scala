/**
 * @author  e.e d3si9n
 */

import purchaseorder._

object XsdTest {
  def main(args: Array[String]) = {
    testUSAddress
  }
  
  def testUSAddress {
    val subject = <USAddress country="US">
      <name>Foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
      <state>DE</state>
      <zip>19808</zip>
    </USAddress>
    
    val address = USAddress.fromXML(subject)
    address match {
      case USAddress("Foo",
        "1537 Paper Street",
        "Wilmington",
        "DE",
        zipCode) => if (zipCode != BigDecimal(19808))
          throw new Exception("testUSAddress: " + address.toString)
      case _ => throw new Exception("testUSAddress: " + address.toString)
    }
    
    println(address.toString)
  }
}
