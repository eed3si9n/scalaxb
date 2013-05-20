package scalaxb.specs

import java.io.{File}
import scalaxb.compiler.{Config}
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
      """val document = <xs:schema targetNamespace="http://www.example.com/IPO"
              xmlns:xs="http://www.w3.org/2001/XMLSchema"
              xmlns:ipo="http://www.example.com/IPO">
        <xs:complexType name="Address">
          <xs:sequence>
            <xs:element name="name"   type="xs:string"/>
            <xs:element name="street" type="xs:string"/>
            <xs:element name="city"   type="xs:string"/>
          </xs:sequence>
          <xs:attribute name="attr" type="xs:string"/>
        </xs:complexType>
      </xs:schema>""", // "
      """toXML[XSchema](fromXML[XSchema](document),
        Some("http://www.w3.org/2001/XMLSchema"), Some("schema"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<xs:schema targetNamespace="http://www.example.com/IPO" """ +
       """xmlns:ipo="http://www.example.com/IPO" xmlns:xs="http://www.w3.org/2001/XMLSchema">""" +
       """<xs:complexType name="Address">""" +
       """<xs:sequence><xs:element type="xs:string" name="name"/>""" +
       """<xs:element type="xs:string" name="street"/>""" +
       """<xs:element type="xs:string" name="city"/>""" +
       """</xs:sequence>""" +
       """<xs:attribute type="xs:string" name="attr"/>""" +
       """</xs:complexType></xs:schema>""", // "
     outdir = "./tmp")
  }
}
