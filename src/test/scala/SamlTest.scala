import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

class SamlTest extends SpecificationWithJUnit with CompilerMatcher {
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
  
  lazy val generated = module.processFiles(
    List((xmldsigcoreschemaxsd, xmldsigcoreschemascala),
      (xencschemaxsd, xencschemascala),
      (samlschemaassertion2xsd, samlschemaassertion2scala),
      (samlschemametadata2xsd, samlschemametadata2scala)),
    Map[String, Option[String]]((null, Some("saml")))  
      )
  
  "xmldsig-core-schema.scala file must compile" in {
    generated must compile(outdir = "./tmp")
  }
  
}
