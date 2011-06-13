/**
 * @author  e.e d3si9n
 */

import scalaxb._
import general._

object GeneralUsage {
  val NS = Some("http://www.example.com/general")
  val O = Some("http://www.example.com/other")

  object Int_ {
    def unapply(x: BigInt) =
      if (x >= Int.MinValue && x <= Int.MaxValue) Some(x.toInt)
      else None

    def unapply(x: BigDecimal) =
      if (x >= Int.MinValue && x <= Int.MaxValue) Some(x.toInt)
      else None
  }
  
  val scope = toScope(None -> "http://www.example.com/general",
    Some("gen") -> "http://www.example.com/general",
    Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance",
    Some("xs") -> "http://www.w3.org/2001/XMLSchema")
    
  def main(args: Array[String]) = {
    allTests
  }

  def allTests = {
    testSingularBuiltInType
    testSingularSimpleType
    testList
    testSingularComplexType
    testChoiceComplexType
    testAny
    testLongAll
    testLongAttribute
    testTopLevelMultipleSeq
    testTopLevelOptionalSeq
    testTopLevelMustipleSeqAny
    testSimpleAnyTypeExtension
    testDataRecord
    true
  }
  
  def testSingularBuiltInType {
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <int>1</int>
      <byte>1</byte>
      <short>1</short>
      <long>1</long>
      <float>1.0</float>
      <double>1.0</double>
      <integer>1</integer>
      <nonPositiveInteger>-1</nonPositiveInteger>
      <negativeInteger>-1</negativeInteger>
      <nonNegativeInteger>1</nonNegativeInteger>
      <positiveInteger>1</positiveInteger>
      <unsignedLong>1</unsignedLong>
      <unsignedInt>1</unsignedInt>
      <unsignedShort>1</unsignedShort>
      <unsignedByte>1</unsignedByte>
      <decimal>1</decimal>
      <boolean>false</boolean>
      <string>foo</string>
      <normalizedString>foo</normalizedString>
      <token>foo</token>
      <language>en-US</language>
      <Name>foo</Name>
      <NCName>foo</NCName>
      <NMTOKEN>foo</NMTOKEN>
      <NMTOKENS>foo</NMTOKENS>
      <ID>foo</ID>
      <IDREF>foo</IDREF>
      <IDREFS>foo</IDREFS>
      <ENTITY>foo</ENTITY>
      <ENTITIES>foo</ENTITIES>
      <anyType xsi:type="xs:string">foo</anyType>
      <anySimpleType xsi:type="xs:string">foo</anySimpleType>
    </foo>
    
    val obj = fromXML[SingularBuiltInTypeTest](subject)
    def check(obj: Any) = obj match {
        case SingularBuiltInTypeTest(
          SingularBuiltInTypeTestSequence1(1, 1, 1, 1, 1.0F, 1.0, Int_(1), Int_(-1), Int_(-1), Int_(1)),
          SingularBuiltInTypeTestSequence2(Int_(1), Int_(1), 1, 1, 1, Int_(1), false, "foo", "foo", "foo"),
          SingularBuiltInTypeTestSequence3("en-US", "foo", "foo", "foo",  Seq("foo"),
            "foo", "foo", Seq("foo"), "foo", Seq("foo")),
          SingularBuiltInTypeTestSequence4(DataRecord(_, _, "foo"), DataRecord(_, _, "foo"))
          ) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[SingularBuiltInTypeTest](obj, "foo", defaultScope)
    println(document)
    check(fromXML[SingularBuiltInTypeTest](document))
  }
      
  def testSingularSimpleType {
    println("testSingularSimpleType")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <number1>1</number1>
      <number2 xsi:nil="true"/>
      <number4>1</number4>
      <number5>2</number5><number5>1</number5>
      
      <milk1>WHOLE</milk1>
      <milk2 xsi:nil="true"/>
      <milk5>WHOLE</milk5><milk5>SKIM</milk5>
    </foo>
    val obj = fromXML[SingularSimpleTypeTest](subject)
    
    def check(obj: Any) = obj match {
        case SingularSimpleTypeTest(1, None, None, Some(Some(1)), Seq(2, 1), Seq(), 
          WHOLE, None, None, None, Seq(WHOLE, SKIM), Seq(),
          None, None) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[SingularSimpleTypeTest](obj, "foo", defaultScope)
    println(document)
    check(fromXML[SingularSimpleTypeTest](document))
  }
  
  def testList {
    println("testList")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <numbers1>1 2 3</numbers1>
      <numbers2 xsi:nil="true"/>
      <numbers4>1</numbers4>
      <numbers5></numbers5><numbers5>1</numbers5>
      <numbers6 xsi:nil="true"/>
      <numbers7>1 2 3</numbers7>
      
      <milk1>WHOLE</milk1>
      <milk2 xsi:nil="true"/>
      <milk5></milk5><milk5>SKIM</milk5>
      <milk6 xsi:nil="true"/>
      <milk7>WHOLE</milk7>
    </foo>
    val obj = fromXML[ListTest](subject)
    
    def check(obj: Any) = obj match {
        case ListTest(Seq(1, 2, 3), None, None, Some(Some(Seq(1))), Seq(Seq(), Seq(1)), Seq(None), Seq(1, 2, 3),
          Seq(WHOLE), None, None, None, Seq(Seq(), Seq(SKIM)), Seq(None), Seq(WHOLE),
          None, None) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[ListTest](obj, "foo", defaultScope)
    check(fromXML[ListTest](document))
    println(document)
  }
  
  def testSingularComplexType {
    println("testSingularComplexType")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <person1><firstName>John</firstName><lastName>Doe</lastName></person1>
      <person2 xsi:nil="true"/>
      <person3><firstName>John</firstName><lastName>Doe</lastName></person3>
      <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
        <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
      <person6 xsi:nil="true"/>
    </foo>
    val obj = fromXML[SingularComplexTypeTest](subject)
    
    def check(obj: Any) = obj match {
        case SingularComplexTypeTest(Person("John", "Doe"), None, Some(Person("John", "Doe")), None,
          Seq(Person("John", "Doe"), Person("John", "Doe")),
          Seq(None)) =>
        case _ => error("match failed: " + obj.toString)
      }
    
    check(obj)
    val document = toXML[SingularComplexTypeTest](obj, "foo", defaultScope)
    check(fromXML[SingularComplexTypeTest](document))
    println(document)
  }
  
  def testChoiceComplexType {
    println("testChoiceComplexType")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <person1><firstName>John</firstName><lastName>Doe</lastName></person1>
      <person2 xsi:nil="true"/>
      <person3><firstName>John</firstName><lastName>Doe</lastName></person3>
      <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
        <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
      <person6 xsi:nil="true"/>
    </foo>
    val obj = fromXML[ChoiceComplexTypeTest](subject)
    
    def check(obj: Any) = obj match {
        case ChoiceComplexTypeTest(
          DataRecord(NS, Some("person1"), Person("John", "Doe")),
          DataRecord(NS, Some("person2"), None),
          Some(DataRecord(NS, Some("person3"), Person("John", "Doe") )),
          None,
          Seq(DataRecord(NS, Some("person5"), Person("John", "Doe")),
            DataRecord(NS, Some("person5"), Person("John", "Doe"))),
          Seq(DataRecord(NS, Some("person6"), None)) ) =>
        case _ => error("match failed: " + obj.toString)
      }
    
    check(obj)
    val document = toXML[ChoiceComplexTypeTest](obj, "foo", defaultScope)
    check(fromXML[ChoiceComplexTypeTest](document))
    println(document)
  }
  
  /*
  <xs:complexType name="AnyTest">
    <xs:sequence>
      <xs:element name="person1" type="gen:Person"/>
      <xs:any namespace="##other" processContents="lax"/>
      <xs:choice>
        <xs:element name="person2" nillable="true" type="gen:Person"/>
        <xs:any namespace="##other" processContents="lax"/>
      </xs:choice>
      <xs:element name="person3" minOccurs="0" type="gen:Person"/>
      <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
      <xs:choice>
        <xs:element name="person4" minOccurs="0" nillable="true" type="gen:Person"/>
        <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
      </xs:choice>
      <xs:any namespace="##other" processContents="lax" maxOccurs="unbounded"/>
      <xs:element name="person5" maxOccurs="unbounded" type="gen:Person"/>
    </xs:sequence>
  </xs:complexType>
  */
  def testAny {
    println("testAny")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:o="http://www.example.com/other">
      <person1><firstName>John</firstName><lastName>Doe</lastName></person1>
      <o:foo xsi:type="xs:int">1</o:foo>
      <person2 xsi:nil="true"/>
      <person3><firstName>John</firstName><lastName>Doe</lastName></person3>
      <o:foo xsi:type="xs:int">1</o:foo>
      <o:foo xsi:type="xs:int">1</o:foo>
      <o:foo xsi:type="xs:int">1</o:foo><o:foo xsi:type="xs:int">1</o:foo>
      <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
      <person5><firstName>John</firstName><lastName>Doe</lastName></person5>
    </foo>
    val obj = fromXML[AnyTest](subject)
    
    def check(obj: Any) = obj match {
        case AnyTest(
          Person("John", "Doe"),
          DataRecord(O, Some("foo"), 1), // Single
          DataRecord(NS, Some("person2"), None),
          Some(Person("John", "Doe")),
          Some(DataRecord(O, Some("foo"), 1)), // optional
          Some(DataRecord(O, Some("foo"), Some(1))), // nillable optional
          Seq(DataRecord(O, Some("foo"), 1), // multiple
            DataRecord(O, Some("foo"), 1)),
          Seq(Person("John", "Doe"), Person("John", "Doe"))
           ) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val scope = toScope(None -> "http://www.example.com/general",
      Some("gen") -> "http://www.example.com/general",
      Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance",
      Some("xs") -> "http://www.w3.org/2001/XMLSchema",
      Some("o") -> "http://www.example.com/other")
    val document = toXML[AnyTest](obj, "foo", scope)
    println(document)
    check(fromXML[AnyTest](document))
  }
    
  def testLongAll {
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <string3></string3><string4></string4><string5></string5>
      <string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>
      <string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>
      <address1><street>1 Plaza</street><city>New York</city><state>NY</state></address1>
      <string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>
      <string21></string21><string22></string22><string23></string23><string24></string24><string25></string25> 
      <string26></string26><string27></string27><string28></string28><string29></string29><string30></string30>
    </foo>
    val obj = fromXML[LongAllTest](subject)
    obj.address1.street match {
      case "1 Plaza" =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML[LongAllTest](obj, "foo", defaultScope)
    println(document)
  }
  
  def testLongAttribute {
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        milk1="SKIM" />
    val obj = fromXML[LongAttributeTest](subject)
    obj.milk1 match {
      case Some(SKIM) =>
      case _ => error("match failed: " + obj.toString)
    }
    
    val document = toXML[LongAttributeTest](obj, "foo", defaultScope)
    println(document)    
  }
  
  def testTopLevelMultipleSeq {
    println("testTopLevelMultipleSeq")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:gen="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    </foo>
    val obj = fromXML[TopLevelMultipleSeqTest](subject)
    
    def check(obj: Any) = obj match {
        case TopLevelMultipleSeqTest() =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[TopLevelMultipleSeqTest](obj, "foo", scope)
    println(document)
    check(fromXML[TopLevelMultipleSeqTest](document))    
  }
  
  def testTopLevelOptionalSeq {
    println("testTopLevelOptionalSeq")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:gen="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    </foo>
    val obj = fromXML[TopLevelOptionalSeqTest](subject)
    
    def check(obj: Any) = obj match {
        case TopLevelOptionalSeqTest(None) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[TopLevelOptionalSeqTest](obj, "foo", scope)
    println(document)
    check(fromXML[TopLevelOptionalSeqTest](document))    
  }
  
  def testTopLevelMustipleSeqAny {
    println("testTopLevelMustipleSeqAny")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:gen="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">something</foo>
    val obj = fromXML[TopLevelMultipleSeqAnyTest](subject)
    
    def check(obj: Any) = obj match {
        case TopLevelMultipleSeqAnyTest(Seq(DataRecord(None, None, "something"))) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[TopLevelMultipleSeqAnyTest](obj, "foo", scope)
    println(document)
    check(fromXML[TopLevelMultipleSeqAnyTest](document))    
  }

  def testSimpleAnyTypeExtension {
    println("testSimpleAnyTypeExtension")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:gen="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xsi:type="xs:string">something</foo>
    val obj = fromXML[AnySimpleTypeExtension](subject)

    def check(obj: Any) = obj match {
        case AnySimpleTypeExtension(DataRecord(_, _, "something"), _) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[AnySimpleTypeExtension](obj, "foo", scope)
    println(document)
    check(fromXML[AnySimpleTypeExtension](document))
  }

  def testDataRecord {
    println("testDataRecord")
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:gen="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"><firstName>John</firstName><lastName>Doe</lastName></foo>
    val obj = fromXML[DataRecord[Person]](subject)
    val scopeList = fromScope(subject.scope)

    scopeList match {
      case (None, "http://www.example.com/general") :: xs =>
      case _ => error(scopeList.toString)
    }

    def check(obj: Any) = obj match {
        case DataRecord(_, Some("foo"), Person("John", "Doe")) =>
        case _ => error("match failed: " + obj.toString)
      }
    check(obj)
    val document = toXML[DataRecord[Person]](obj, "foo", toScope(scopeList: _*))
    println(document)
    check(fromXML[DataRecord[Person]](document))
  }
}
