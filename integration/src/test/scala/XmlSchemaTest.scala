import java.io.{File}
import scalaxb.compiler.{Config, Verbose}
import scalaxb.compiler.xsd.{Driver}

object XmlSchemaTest extends TestBase {
  // override val module = new Driver with Verbose
  val inFile  = new File("integration/src/test/resources/xmlschema.xsd")
  lazy val generated = module.process(inFile,
    Config(packageNames = Map(Some("http://www.w3.org/2001/XMLSchema") -> Some("org.w3.xmlschema")),
      outdir = tmp,
      classPrefix = Some("X"),
      paramPrefix = Some("m")
    ))
    
  "XMLSchema.scala file must compile so that Schema can be used" in {
    (List("import scalaxb._",
      "import org.w3.xmlschema._",
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
       """xmlns="http://www.w3.org/2001/XMLSchema" xmlns:ipo="http://www.example.com/IPO">""" +
       """<complexType name="Address">""" +
       """<sequence><element type="string" name="name"></element>""" +
       """<element type="string" name="street"></element>""" +
       """<element type="string" name="city"></element>""" +
       """</sequence>""" +
       """<attribute type="string" name="attr"></attribute>""" +
       """</complexType></schema>""", // "
     outdir = "./tmp")
  }
}
