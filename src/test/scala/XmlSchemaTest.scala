import java.io.{File}
import org.scalaxb.compiler.{Config}

object XmlSchemaTest extends TestBase {
  val inFile  = new File("src/test/resources/xmlschema.xsd")
  val outFile = new File(tmp, "XMLSchema.scala")
  
  lazy val generated = module.process(inFile, outFile,
    Config(packageNames = Map(Some("http://www.w3.org/2001/XMLSchema") -> Some("org.w3.xmlschema")),
      classPrefix = Some("X"),
      paramPrefix = Some("m")
    ))
    
  "XMLSchema.scala file must compile so that Schema can be used" in {
    (List("import org.scalaxb.rt._",
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
        </complexType>
      </schema>""", // " 
      """toXML[XSchema](fromXML[XSchema](document),
        Some("http://www.w3.org/2001/XMLSchema"), Some("schema"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<schema targetNamespace="http://www.example.com/IPO" """ +
       """xmlns:ipo="http://www.example.com/IPO" """ +
       """xmlns="http://www.w3.org/2001/XMLSchema">""" +
       """<complexType name="Address">""" +
       """<sequence><element type="string"></element>""" + 
       """<element type="string"></element>""" + 
       """<element type="string"></element>""" +
       """</sequence></complexType></schema>""", // "
     outdir = "./tmp")
  }
}
