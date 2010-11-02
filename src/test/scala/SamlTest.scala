import java.io.{File}
import scalaxb.compiler.{Config}

object SamlTest extends TestBase {
  val xmldsigcoreschemaxsd = new File("src/test/resources/xmldsig-core-schema.xsd")
  val xencschemaxsd = new File("src/test/resources/xenc-schema.xsd")
  val samlschemaassertion2xsd = new File("src/test/resources/saml-schema-assertion-2.0.xsd")
  val samlschemametadata2xsd = new File("src/test/resources/saml-schema-metadata-2.0.xsd")
  
  val xmldsigcoreschemascala = new File(tmp, "xmldsig-core-schema.scala")
  val xencschemascala = new File(tmp, "xenc-schema.scala")
  val samlschemaassertion2scala = new File(tmp, "saml-schema-assertion-2.scala")
  val samlschemametadata2scala = new File(tmp, "saml-schema-metadata-2.scala")
  val usageFile = new File(tmp, "SamlUsage.scala")
  copyFileFromResource("SamlUsage.scala", usageFile)
  
  lazy val generated = module.processFiles(
    List(xmldsigcoreschemaxsd -> xmldsigcoreschemascala,
      xencschemaxsd -> xencschemascala,
      samlschemaassertion2xsd -> samlschemaassertion2scala,
      samlschemametadata2xsd -> samlschemametadata2scala),
    Config(packageNames = Map(None -> Some("saml"),
      Some("http://www.w3.org/2000/09/xmldsig#") -> Some("org.w3.xmldsig"),
      Some("http://www.w3.org/2001/04/xmlenc#") -> Some("org.w3.xmlenc"),
      Some("urn:oasis:names:tc:SAML:2.0:assertion") -> Some("org.xml.saml2.assertion"),
      Some("urn:oasis:names:tc:SAML:2.0:metadata") -> Some("org.xml.saml2.metadata")  
      )) )
  
  "generated files must compile together with SamlUsage.scala" in {
    (List("SamlUsage.allTests"),
     usageFile :: generated) must evaluateTo(true,
       outdir = "./tmp")
  }
}
