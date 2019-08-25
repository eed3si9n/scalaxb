import java.io.{File}
import scalaxb.compiler.{Config, Log}
import scalaxb.compiler.ConfigEntry._

class ImportTest extends TestBase {
  // Log.configureLogger(true)
  val ipoxsd    = new File("integration/src/test/resources/ipo.xsd")
  val reportxsd = new File("integration/src/test/resources/report.xsd")
  val circularxsd = new File("integration/src/test/resources/circular.xsd")
  val conflictxsd = new File("integration/src/test/resources/conflict.xsd")
  val includexsd = new File("integration/src/test/resources/include.xsd")

  val config = Config.default.update(PackageNames(
      Map(None -> Some("ipo"),
        Some("http://www.example.com/Report") -> Some("org.report")))).
    update(Outdir(tmp)).
    update(GeneratePackageDir)
  lazy val generated = module.processFiles(
    List(ipoxsd, reportxsd, circularxsd, conflictxsd, includexsd),
    config)
  "report.xsd must generate report.scala file" in {
    (generated(0) must exist) and
    (generated(1) must exist)
  }

  "report.scala file must compile so that PurchaseReport can be used" in {
    (List("import ipo._",
          "import org.report._",
          "PurchaseReport(RegionsType(), PartsType(), Map()).toString"),
     generated) must evaluateTo("PurchaseReport(RegionsType(List()),PartsType(List()),Map())", outdir = "./tmp")
  }
}
