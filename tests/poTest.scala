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
    val subject = <USAddress country="US">
      <name>Foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
      <state>DE</state>
      <zip>19808</zip>
    </USAddress>
    
    val address = USAddress.fromXML(subject)
    address match {
      case USAddress("US",
        "Foo",
        "1537 Paper Street",
        "Wilmington",
        "DE",
        zipCode) =>
          if (zipCode != BigDecimal(19808))
            throw new Exception("value doesn't match: " + address.toString)
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
      case Item("639-OS",
        "Olive Soap",
        quantity,
        usPrice,
        None,
        Some(Calendar("2010-02-06T00:00:00.000Z"))) =>
          if (usPrice != BigDecimal(4.00) || quantity != BigInt(1))
            throw new Exception("values don't match: " + item.toString)
      case _ => throw new Exception("match failed: " + item.toString)
    }
    
    println(item.toString)
  }
}
