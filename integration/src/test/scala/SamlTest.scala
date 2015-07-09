import java.io.{File}
import scalaxb.compiler.{Config}

object SamlTest extends TestBase {
  val xmldsigcoreschemaxsd = new File("integration/src/test/resources/xmldsig-core-schema.xsd")
  val xencschemaxsd = new File("integration/src/test/resources/xenc-schema.xsd")
  val samlschemaassertion2xsd = new File("integration/src/test/resources/saml-schema-assertion-2.0.xsd")
  val samlschemametadata2xsd = new File("integration/src/test/resources/saml-schema-metadata-2.0.xsd")
  val usageFile = new File(tmp, "SamlUsage.scala")
  copyFileFromResource("SamlUsage.scala", usageFile)
  
  lazy val generated = module.processFiles(
    List(xmldsigcoreschemaxsd, xencschemaxsd, samlschemaassertion2xsd, samlschemametadata2xsd),
    new Config(packageNames = Map(None -> Some("saml"),
      Some("http://www.w3.org/2000/09/xmldsig#") -> Some("org.w3.xmldsig"),
      Some("http://www.w3.org/2001/04/xmlenc#") -> Some("org.w3.xmlenc"),
      Some("urn:oasis:names:tc:SAML:2.0:assertion") -> Some("org.xml.saml2.assertion"),
      Some("urn:oasis:names:tc:SAML:2.0:metadata") -> Some("org.xml.saml2.metadata")),
      outdir = tmp) )
  
  "generated files must compile together with SamlUsage.scala" in {
    (List("SamlUsage.allTests"),
     usageFile :: generated) must evaluateTo(true,
       outdir = "./tmp")
  }
}
