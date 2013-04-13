/**
 * @author  e.e d3si9n
 */

import scalaxb._
import anycontent._

object AnyUsage {
  def main(args: Array[String]) = {
    allTests
  }

  def allTests = {
    testAny
    testAny2
    testAny3
    true
  }
  
  def testAny {
    val subject = <foo xmlns="http://www.example.com/any"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <int xsi:type="xs:int">1</int>
          <byte xsi:type="xs:byte">1</byte>
          <short xsi:type="xs:short">1</short>
          <long xsi:type="xs:long">1</long>
          <float xsi:type="xs:float">1.0</float>
          <double xsi:type="xs:double">1.0</double>
          <integer xsi:type="xs:integer">1</integer>
          <nonPositiveInteger xsi:type="xs:nonPositiveInteger">-1</nonPositiveInteger>
          <negativeInteger xsi:type="xs:negativeInteger">-1</negativeInteger>
          <nonNegativeInteger xsi:type="xs:nonNegativeInteger">1</nonNegativeInteger>
          <positiveInteger xsi:type="xs:positiveInteger">1</positiveInteger>
          <unsignedLong xsi:type="xs:unsignedLong">1</unsignedLong>
          <unsignedInt xsi:type="xs:unsignedInt">1</unsignedInt>
          <unsignedShort xsi:type="xs:unsignedShort">1</unsignedShort>
          <unsignedByte xsi:type="xs:unsignedByte">1</unsignedByte>
          <decimal xsi:type="xs:decimal">1</decimal>
        </foo>
    val obj = fromXML[Element1](subject)
    obj match {
      case Element1(
          DataRecord(Some("http://www.example.com/any"), Some("int"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("byte"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("short"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("long"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("float"), 1.0F),
          DataRecord(Some("http://www.example.com/any"), Some("double"), 1.0),
          DataRecord(Some("http://www.example.com/any"), Some("integer"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("nonPositiveInteger"), -1),
          DataRecord(Some("http://www.example.com/any"), Some("negativeInteger"), -1),
          DataRecord(Some("http://www.example.com/any"), Some("nonNegativeInteger"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("positiveInteger"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("unsignedLong"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("unsignedInt"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("unsignedShort"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("unsignedByte"), 1),
          DataRecord(Some("http://www.example.com/any"), Some("decimal"), 1)
        ) =>
      case _ => sys.error("match failed: " + obj.toString)
    }
    val document = toXML[Element1](obj, "foo", defaultScope)
    println(document)
  }

  def testAny2 {
    val subject = <foo xmlns="http://www.example.com/any"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <boolean xsi:type="xs:boolean">false</boolean>
          <string xsi:type="xs:string">foo</string>
          <normalizedString xsi:type="xs:normalizedString">foo</normalizedString>
          <token xsi:type="xs:token">foo</token>
          <language xsi:type="xs:language">en-US</language>
          <Name xsi:type="xs:Name">foo</Name>
          <NCName xsi:type="xs:NCName">foo</NCName>
          <NMTOKEN xsi:type="xs:NMTOKEN">foo</NMTOKEN>
          <NMTOKENS xsi:type="xs:NMTOKENS">foo</NMTOKENS>
          <ID xsi:type="xs:ID">foo</ID>
          <IDREF xsi:type="xs:IDREF">foo</IDREF>
          <IDREFS xsi:type="xs:IDREFS">foo</IDREFS>
          <ENTITY xsi:type="xs:ENTITY">foo</ENTITY>
          <ENTITIES xsi:type="xs:ENTITIES">foo</ENTITIES>
        </foo>
    val obj = fromXML[Element1](subject)
    obj match {
      case Element1(
          DataRecord(Some("http://www.example.com/any"), Some("boolean"), false),
          DataRecord(Some("http://www.example.com/any"), Some("string"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("normalizedString"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("token"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("language"), "en-US"),
          DataRecord(Some("http://www.example.com/any"), Some("Name"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("NCName"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("NMTOKEN"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("NMTOKENS"), Seq("foo")),
          DataRecord(Some("http://www.example.com/any"), Some("ID"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("IDREF"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("IDREFS"), Seq("foo")),
          DataRecord(Some("http://www.example.com/any"), Some("ENTITY"), "foo"),
          DataRecord(Some("http://www.example.com/any"), Some("ENTITIES"), Seq("foo"))
        ) =>
      case _ => sys.error("match failed: " + obj.toString)
    }
    val document = toXML[Element1](obj, "foo", defaultScope)
    println(document)
  }
  
  def testAny3 {
    val ExampleCom = new java.net.URI("http://www.example.com/")
    val ExampleQName = javax.xml.namespace.QName.valueOf("{http://www.example.com/any}foo")
    lazy val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()
    val ExampleDuration = typeFactory.newDuration("P1D")
    
    val subject = <foo xmlns="http://www.example.com/any"
        xmlns:any="http://www.example.com/any"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <hexBinary xsi:type="xs:hexBinary">0F</hexBinary>
          <base64Binary xsi:type="xs:base64Binary">bQ==</base64Binary>
          <anyURI xsi:type="xs:anyURI">http://www.example.com/</anyURI>
          <QName xsi:type="xs:QName">any:foo</QName>
          <NOTATION xsi:type="xs:NOTATION">{{http://www.example.com/any}}foo</NOTATION>
          <duration xsi:type="xs:duration">P1D</duration>
          <dateTime xsi:type="xs:dateTime">2002-10-10T12:00:00Z</dateTime>
          <time xsi:type="xs:time">12:00:00Z</time>
          <gYearMonth xsi:type="xs:gYearMonth">2002-10</gYearMonth>
          <gYear xsi:type="xs:gYear">2002</gYear>
          <gMonthDay xsi:type="xs:gMonthDay">--10-10</gMonthDay>
          <gDay xsi:type="xs:gDay">---10</gDay>
          <gMonth xsi:type="xs:gMonth">--10</gMonth>
        </foo>
    
    val obj = fromXML[Element1](subject)
    obj match {
      case Element1(
          DataRecord(Some("http://www.example.com/any"), Some("hexBinary"), HexBinary(15)),
          DataRecord(Some("http://www.example.com/any"), Some("base64Binary"), Base64Binary('m')),
          DataRecord(Some("http://www.example.com/any"), Some("anyURI"), ExampleCom),
          DataRecord(Some("http://www.example.com/any"), Some("QName"), ExampleQName),
          DataRecord(Some("http://www.example.com/any"), Some("NOTATION"), ExampleQName),
          DataRecord(Some("http://www.example.com/any"), Some("duration"), ExampleDuration),
          DataRecord(Some("http://www.example.com/any"), Some("dateTime"), XMLCalendar("2002-10-10T12:00:00Z")),
          DataRecord(Some("http://www.example.com/any"), Some("time"), XMLCalendar("12:00:00Z")),
          DataRecord(Some("http://www.example.com/any"), Some("gYearMonth"), XMLCalendar("2002-10")),
          DataRecord(Some("http://www.example.com/any"), Some("gYear"), XMLCalendar("2002")),
          DataRecord(Some("http://www.example.com/any"), Some("gMonthDay"), XMLCalendar("--10-10")),
          DataRecord(Some("http://www.example.com/any"), Some("gDay"), XMLCalendar("---10")),
          DataRecord(Some("http://www.example.com/any"), Some("gMonth"), XMLCalendar("--10"))
        ) =>
      case _ => sys.error("match failed: " + obj.toString)
    }
    val document = toXML[Element1](obj, "foo", defaultScope)
    println(document)
  }
}
