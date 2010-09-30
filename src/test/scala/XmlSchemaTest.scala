import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object XmlSchemaTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val xmlschemaxsd = new File("src/test/resources/xmlschema.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val xmlschemascala = new File(tmp, "XMLSchema.scala")
    
  lazy val generated = module.processFiles(
    List((xmlschemaxsd, xmlschemascala)),
    Map[Option[String], Option[String]](
      Some("http://www.w3.org/2001/XMLSchema") -> Some("org.w3.xmlschema")
    ))
    
  "XMLSchema.scala file must compile so that Schema can be used" in {
    (List("import org.w3.xmlschema._",
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
      """Schema.toXML(Schema.fromXML(document),
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
