import java.io.{File}
import scalaxb.compiler.{Config, Verbose}
import scalaxb.compiler.xsd.{Driver}

object XmlSchemaTest extends TestBase {
  // override val module = new Driver with Verbose
  val inFile  = new File("cli/src/test/resources/xmlschema.xsd")
  val outFile = new File(tmp, "XMLSchema.scala")
  
  lazy val generated = module.process(inFile, outFile, outProtocolFile,
    Config(packageNames = Map(Some("http://www.w3.org/2001/XMLSchema") -> Some("org.w3.xmlschema")),
      classPrefix = Some("X"),
      paramPrefix = Some("m")
    ))
    
  "XMLSchema.scala file must compile so that Schema can be used" in {
    (List("import scalaxb._",
      "import Scalaxb._",
      "import org.w3.xmlschema._",
      "import XDefaultXMLProtocol._",
      """val document = <schema targetNamespace="http://www.example.com/IPO"
              xmlns="http://www.w3.org/2001/XMLSchema"
              xmlns:ipo="http://www.example.com/IPO">
        <complexType name="Address">
          <sequence>
            <element name="name"   type="string"/>
            <element name="street" type="string"/>
            <element name="city"   type="string"/>
          </sequence>
          <attribute name="attr" type="string"/>
        </complexType>
      </schema>""", // " 
      """toXML[XSchema](fromXML[XSchema](document),
        Some("http://www.w3.org/2001/XMLSchema"), Some("schema"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<schema targetNamespace="http://www.example.com/IPO" """ +
       """xmlns:ipo="http://www.example.com/IPO" """ +
       """xmlns="http://www.w3.org/2001/XMLSchema">""" +
       """<complexType name="Address">""" +
       """<sequence><element name="name" type="string"></element>""" +
       """<element name="street" type="string"></element>""" +
       """<element name="city" type="string"></element>""" +
       """</sequence>""" +
       """<attribute name="attr" type="string"></attribute>""" +
       """</complexType></schema>""", // "
     outdir = "./tmp")
  }
}
