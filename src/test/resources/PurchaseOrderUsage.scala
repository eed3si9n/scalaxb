/**
 * @author  e.e d3si9n
 */

import scalaxb._
import Scalaxb._
import ipo._
import DefaultXMLProtocol._

object PurchaseOrderUsage {
  def main(args: Array[String]) = {
    allTests
  }

  def allTests = {
    testUSAddress
    testItem
    testItems
    testPurchaseOrder
    testTimeOlson
    testIntWithAttr
    testChoices
    testLangAttr
    testRoundTrip
    testChoiceRoundTrip
    testAny
    testAnyChoice
    testAnyAttribute
    testDatedData
    testNillable
    testAll
    testContentModel
    testSubstitutionGroup
    
    true
  }
  
  def testUSAddress {
    val subject = <shipTo xmlns="http://www.example.com/IPO"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:ipo="http://www.example.com/IPO"
        xsi:type="ipo:USAddress">
      <name>Foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
      <state>DE</state>
      <zip>19808</zip>
    </shipTo>
    
    val address = fromXML[Addressable](subject)
    address match {
      case USAddress("Foo",
        "1537 Paper Street",
        "Wilmington",
        DE,
        19808) =>
      case _ => error("match failed: " + address.toString)
    }
    
    println(address.toString)
  }
  
  def testItem {
    val subject = <item partNum="639-OS" xmlns="http://www.example.com/IPO">
      <productName>Olive Soap</productName>
      <quantity>1</quantity>
      <USPrice>4.00</USPrice>
      <shipDate>2010-02-06Z</shipDate>
    </item>
    
    val item = fromXML[Item](subject)
    item match {
      case Item("Olive Soap",
        1,
        usPrice,
        None,
        Some(XMLCalendar("2010-02-06Z")),
        "639-OS") =>
          if (usPrice != BigDecimal(4.00))
            error("values don't match: " + item.toString)
      case _ => error("match failed: " + item.toString)
    }
    
    println(item.toString)
  }
  
  def testItems {
    val subject = <items xmlns="http://www.example.com/IPO">
      <item partNum="639-OS">
        <productName>Olive Soap</productName>
        <quantity>1</quantity>
        <USPrice>4.00</USPrice>
        <shipDate>2010-02-06Z</shipDate>
      </item>
    </items>
    
    val items = fromXML[Items](subject)
    items match {
      case Items(_) =>
      case _ => error("match failed: " + items.toString)
    }    
    println(items.toString)    
    
  }
  
  def testPurchaseOrder {
    val subject = <purchaseOrder
        xmlns="http://www.example.com/IPO"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:ipo="http://www.example.com/IPO"
        orderDate="1999-12-01Z">
      <shipTo exportCode="1" xsi:type="ipo:UKAddress">
        <name>Helen Zoe</name>
        <street>47 Eden Street</street>
        <city>Cambridge</city>
        <postcode>CB1 1JR</postcode>
      </shipTo>
      <billTo xsi:type="ipo:USAddress">
        <name>Foo</name>
        <street>1537 Paper Street</street>
        <city>Wilmington</city>
        <state>DE</state>
        <zip>19808</zip>   
      </billTo>
      <items>
        <item partNum="639-OS">
          <productName>Olive Soap</productName>
          <quantity>1</quantity>
          <USPrice>4.00</USPrice>
          <shipDate>2010-02-06Z</shipDate>
        </item>
      </items>
    </purchaseOrder>
    
    val purchaseOrder = fromXML[PurchaseOrderType](subject)
    purchaseOrder match {
      case PurchaseOrderType(
        shipTo: UKAddress,
        billTo: USAddress,
        None,
        Items(_),
        Some(XMLCalendar("1999-12-01Z"))) =>
      case _ => error("match failed: " + purchaseOrder.toString)
    }    
    println(purchaseOrder.toString)  
  }
  
  def testTimeOlson {
    val subject = <time xmlns="http://www.example.com/IPO">00:00:00</time>
    
    val timeOlson = fromXML[TimeOlson](subject)
    timeOlson match {
      case TimeOlson(XMLCalendar("00:00:00"),
        "") =>
      case _ => error("match failed: " + timeOlson.toString)
    }
    
    println(timeOlson.toString)
  }
  
  def testIntWithAttr {
    val subject = <some foo="test" xmlns="http://www.example.com/IPO">1</some>
    
    val intWithAttr = fromXML[IntWithAttr](subject)
    intWithAttr match {
      case IntWithAttr(1, "test") =>
      case _ => error("match failed: " + intWithAttr.toString)
    }
    
    println(intWithAttr.toString)    
  }

  def testChoices {
    val subject = <Element1 xmlns="http://www.example.com/IPO"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:ipo="http://www.example.com/IPO">
      <Choice2>1</Choice2>
    </Element1>
    
    val obj = fromXML[Element1](subject)
    obj match {
      case Element1(DataRecord(Some("http://www.example.com/IPO"), Some("Choice2"), 1)) =>
      case _ => error("match failed: " + obj.toString)
    }

    println(obj.toString)
  }

  def testLangAttr {
    val subject = <Choice1 xml:lang="en" xmlns="http://www.example.com/IPO"></Choice1>
    val obj = fromXML[Choice1](subject)
    obj match {
      case Choice1(_, "en", _) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    println(obj.toString)
  }

  def testRoundTrip {
    import scala.xml.{TopScope, NamespaceBinding}
    
    val subject = <shipTo xmlns="http://www.example.com/IPO"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:ipo="http://www.example.com/IPO"
        xsi:type="ipo:USAddress">
      <name>Foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
      <state>DE</state>
      <zip>19808</zip>
    </shipTo>
    
    val obj = fromXML[Addressable](subject)
    obj match {
      case usaddress: USAddress =>
        val document = toXML[Addressable](usaddress, None, Some("shipTo"), subject.scope)
        println(document)
        val obj2 = fromXML[Addressable](document)
        
        obj2 match {
          case `usaddress` =>
          case _ => error("match failed: " + obj2.toString)
        }

        println(obj2.toString)
        
      case _ => error("parsed object is not USAddress") 
    }
  }
  
  def testChoiceRoundTrip {
    val subject = <Element1 xmlns="http://www.example.com/IPO"><Choice2>1</Choice2></Element1>
    val obj = fromXML[Element1](subject)
    val document = toXML(obj, Some("http://www.example.com/IPO"), Some("Element1"), subject.scope)
    println(document)
    val obj2 = fromXML[Element1](document)
    obj2 match {
      case `obj` =>
      case _ => error("match failed: " + obj2.toString)
    }
  }
  
  def testAny {
    val subject = <choice1 xmlns="http://www.example.com/IPO"
        xmlns:ipo="http://www.example.com/IPO"
        xmlns:h="http://www.w3.org/1999/xhtml"
        xml:lang="en"
        h:href="4Q99.html">
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <apply>
          <log/>
          <logbase><cn>3</cn></logbase>
          <ci>x</ci>
        </apply>
      </math>
    </choice1>
    val obj = fromXML[Choice1](subject)
    obj match {
      case Choice1(_, "en",
        Seq(DataRecord(Some("http://www.w3.org/1999/xhtml"), Some("href"), "4Q99.html"))) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("choice1"), subject.scope)
    println(document)  
  }
  
  def testAnyChoice {
    val subject = <Element1 xmlns="http://www.example.com/IPO"
        xmlns:ipo="http://www.example.com/IPO">
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <apply>
          <log/>
          <logbase><cn>3</cn></logbase>
          <ci>x</ci>
        </apply>
      </math>
    </Element1>
    val obj = fromXML[Element1](subject)
    val document = toXML(obj, None, Some("Element1"), subject.scope)
    println(document)
    val obj2 = fromXML[Element1](document)
    obj2 match {
      case Element1(DataRecord(Some("http://www.w3.org/1998/Math/MathML"), Some("math"), _)) =>
      case _ => error("match failed: " + document.toString)
    }    
  }
  
  def testAnyAttribute {
    val subject = <foo xmlns="http://www.example.com/IPO"
        xmlns:ipo="http://www.example.com/IPO"
        xmlns:h="http://www.w3.org/1999/xhtml"
        h:href="4Q99.html">
    </foo>
    val obj = fromXML[Element2](subject)
    obj match {
      case Element2(Seq(DataRecord(Some("http://www.w3.org/1999/xhtml"), Some("href"), "4Q99.html"))) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("foo"), subject.scope)
    println(document)
    val obj2 = fromXML[Element2](document)
    obj2 match {
      case Element2(Seq(DataRecord(Some("http://www.w3.org/1999/xhtml"), Some("href"), "4Q99.html"))) =>
      case _ => error("match failed: " + obj2.toString)
    }
  }
  
  def testDatedData {
    val subject = <foo xmlns="http://www.example.com/IPO"
        xmlns:ipo="http://www.example.com/IPO" id="foo">
      <date>2010-02-06Z</date>
      <hexBinary>0F</hexBinary>
      <base64Binary>QUJDREVGRw==</base64Binary>
    </foo>
    val obj = fromXML[DatedData](subject)
    obj match {
      case DatedData(XMLCalendar("2010-02-06Z"),
        HexBinary(15),
        Array('A', 'B', 'C', 'D', 'E', 'F', 'G'),
        Coreattrs(None, Some("foo"))) =>
      case _ => error("match failed: " + obj.toString)
    }
    val document = toXML(obj, None, Some("foo"), subject.scope)
    println(document)
  }
  
  def testNillable {
    val subject = <foo xmlns="http://www.example.com/IPO"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:ipo="http://www.example.com/IPO">
      <price xsi:nil="true" />
      <tax xsi:nil="true" />
      <tag xsi:nil="true" />
      <tag xsi:nil="true" />
      <shipTo xsi:nil="true" />
      <billTo xsi:nil="true" />
      <via xsi:nil="true" />
      <via xsi:nil="true" />
    </foo>
    val obj = fromXML[NillableTest](subject)
    obj match {
      case NillableTest(None, Some(None), Seq(None, None),
        None, Some(None), Seq(None, None)) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("foo"), subject.scope)
    println(document)
  }
  
  def testAll {
    val subject = <foo xmlns="http://www.example.com/IPO"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:ipo="http://www.example.com/IPO">
      <title>bar</title>
      <style></style>
      <script></script>
    </foo>
    val obj = fromXML[AllTest](subject)
    obj match {
      case AllTest(Some(""), Some(""), Some("bar"), _, None) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("foo"), subject.scope)
    println(document)    
  }
  
  def testContentModel {
    val subject = <head xmlns="http://www.example.com/IPO"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:ipo="http://www.example.com/IPO"
      dir="ltr">
      <script></script>
      <script></script>
      <title>bar</title>
      <script></script>
    </head>
    val obj = fromXML[Head](subject)
    obj match {
      case Head(Seq(DataRecord(Some("http://www.example.com/IPO"), Some("script"), ""),
        DataRecord(Some("http://www.example.com/IPO"), Some("script"), "")),
        DataRecord(None, None, HeadSequence1("bar", Seq(DataRecord(Some("http://www.example.com/IPO"), Some("script"), "")) )),
        I18n(Some(Ltr), None, None), None, None) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("head"), subject.scope)
    println(document) 
  }
  
  def testSubstitutionGroup {
    val subject = <billTo xmlns="http://www.example.com/IPO"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:ipo="http://www.example.com/IPO"
      dir="ltr">
      <gh6sub1>foo</gh6sub1>
      <gh6head2>bar</gh6head2>
      <city>baz</city>
    </billTo>
    val obj = fromXML[GH6Usage](subject)
    obj match {
      case GH6Usage(DataRecord(Some("http://www.example.com/IPO"),
        Some("gh6sub1"), "foo"), "bar", "baz") =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML(obj, None, Some("billTo"), subject.scope)
    println(document)
  }
}
