import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

class ImportTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val ipoxsd    = new File("src/test/resources/ipo.xsd")
  val reportxsd = new File("src/test/resources/report.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val iposcala = new File(tmp, "ipo.scala")
  val reportscala = new File(tmp, "report.scala")
  lazy val purchaseOrderSchema = module.parse(ipoxsd)
  
  lazy val generated = module.processFiles(
    List((ipoxsd, iposcala), (reportxsd, reportscala)),
    Map[String, Option[String]]((null, Some("ipo")))  
      )
  "report.xsd must generate report.scala file" in {
    generated(0) must exist
    generated(1) must exist
  }

  "report.scala file must compile so that PurchaseReport can be used" in {
    (List("import ipo._",
          "PurchaseReport(RegionsType(), PartsType(), None, None).toString"),
     generated) must evaluateTo("PurchaseReport(RegionsType(WrappedArray()),PartsType(WrappedArray()),None,None)", outdir = "./tmp")
  }
}
