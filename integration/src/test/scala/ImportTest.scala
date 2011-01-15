import java.io.{File}
import scalaxb.compiler.{Config}

object ImportTest extends TestBase {
  val ipoxsd    = new File("integration/src/test/resources/ipo.xsd")
  val reportxsd = new File("integration/src/test/resources/report.xsd")
  val circularxsd = new File("integration/src/test/resources/circular.xsd")
  
  lazy val generated = module.processFiles(
    List(ipoxsd, reportxsd, circularxsd),
    Config(packageNames = Map(None -> Some("ipo"),
      Some("http://www.example.com/Report") -> Some("report") ),
      outdir = tmp) )
    
  "report.xsd must generate report.scala file" in {
    generated(0) must exist
    generated(1) must exist
  }

  "report.scala file must compile so that PurchaseReport can be used" in {
    (List("import ipo._",
          "import report._",
          "PurchaseReport(RegionsType(), PartsType(), None, None).toString"),
     generated) must evaluateTo("PurchaseReport(RegionsType(WrappedArray()),PartsType(WrappedArray()),None,None)", outdir = "./tmp")
  }
}
