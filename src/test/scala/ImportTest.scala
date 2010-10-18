import java.io.{File}
import org.scalaxb.compiler.{Config}

object ImportTest extends TestBase {
  val ipoxsd    = new File("src/test/resources/ipo.xsd")
  val reportxsd = new File("src/test/resources/report.xsd")
  val iposcala = new File(tmp, "ipo.scala")
  val reportscala = new File(tmp, "report.scala")
  
  lazy val generated = module.processFiles(
    List(ipoxsd -> iposcala,
      reportxsd -> reportscala),
    Config(packageNames = Map(None -> Some("ipo"),
      Some("http://www.example.com/Report") -> Some("report")
    )) )
    
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
