import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object SamlTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val xmldsigcoreschemaxsd = new File("src/test/resources/xmldsig-core-schema.xsd")
  val xencschemaxsd = new File("src/test/resources/xenc-schema.xsd")
  val samlschemaassertion2xsd = new File("src/test/resources/saml-schema-assertion-2.0.xsd")
  val samlschemametadata2xsd = new File("src/test/resources/saml-schema-metadata-2.0.xsd")
  
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir
  
  val xmldsigcoreschemascala = new File(tmp, "xmldsig-core-schema.scala")
  val xencschemascala = new File(tmp, "xenc-schema.scala")
  val samlschemaassertion2scala = new File(tmp, "saml-schema-assertion-2.scala")
  val samlschemametadata2scala = new File(tmp, "saml-schema-metadata-2.scala")
  val samlUsagescala = new File(tmp, "SamlUsage.scala")
  copyFileFromResource("SamlUsage.scala", samlUsagescala)
  
  lazy val generated = module.processFiles(
    List((xmldsigcoreschemaxsd, xmldsigcoreschemascala),
      (xencschemaxsd, xencschemascala),
      (samlschemaassertion2xsd, samlschemaassertion2scala),
      (samlschemametadata2xsd, samlschemametadata2scala)),
    Map[Option[String], Option[String]](
      None -> Some("saml"),
      Some("http://www.w3.org/2000/09/xmldsig#") -> Some("org.w3.xmldsig"),
      Some("http://www.w3.org/2001/04/xmlenc#") -> Some("org.w3.xmlenc"),
      Some("urn:oasis:names:tc:SAML:2.0:assertion") -> Some("org.xml.saml2.assertion"),
      Some("urn:oasis:names:tc:SAML:2.0:metadata") -> Some("org.xml.saml2.metadata")  
      ), false)
  
  "generated files must compile together with SamlTest.scala" in {
    (List("SamlUsage.allTests"),
     samlUsagescala :: generated) must evaluateTo(true,
       outdir = "./tmp")
  }
}
