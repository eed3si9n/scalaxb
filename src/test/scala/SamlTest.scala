import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

class SamlTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val xmldsigcoreschemaxsd = new File("src/test/resources/xmldsig-core-schema.xsd")
  // val reportxsd = new File("src/test/resources/report.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir
  
  val xmldsigcoreschemascala = new File(tmp, "xmldsig-core-schema.scala")
  // val reportscala = new File(tmp, "report.scala")
  lazy val generated = module.processFiles(
    List((xmldsigcoreschemaxsd, xmldsigcoreschemascala)),
    Map[String, Option[String]]((null, Some("saml")))  
      )
  
  "xmldsig-core-schema.scala exists" in {
    generated(0) must exist
  }
      
  "xmldsig-core-schema.scala file must compile" in {
    generated must compile(outdir = "./tmp")
  }
}
