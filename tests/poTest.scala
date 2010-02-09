/**
 * @author  e.e d3si9n
 */

import purchaseorder._

object XsdTest {
  def main(args: Array[String]) = {
    testUSAddress
    testItem
  }
  
  def testUSAddress {
    val subject = <USAddress>
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
        19808) =>
      case _ => throw new Exception("match failed: " + address.toString)
    }
    
    println(address.toString)
  }
  
  def testItem {
    val subject = <item partNum="639-OS">
      <productName>Olive Soap</productName>
      <quantity>1</quantity>
      <USPrice>4.00</USPrice>
      <shipDate>2010-02-06Z</shipDate>
    </item>
    
    val item = Item.fromXML(subject)
    item match {
      case Item("Olive Soap",
        1,
        usPrice,
        None,
        Some(Calendar("2010-02-06T00:00:00.000Z")),
        "639-OS") =>
          if (usPrice != BigDecimal(4.00))
            throw new Exception("values don't match: " + item.toString)
      case _ => throw new Exception("match failed: " + item.toString)
    }
    
    println(item.toString)
  }
}
