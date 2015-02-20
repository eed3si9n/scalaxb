import java.io.File

import scalaxb.compiler.Config

object IgnoreUnknownPurchaseOrderTest extends TestBase {
  val inFile    = new File("integration/src/test/resources/ipo.xsd")
  val ignoreUnknownUsageFile = new File(tmp, "PurchaseOrderIgnoreUnknownUsage.scala")
  copyFileFromResource("PurchaseOrderIgnoreUnknownUsage.scala", ignoreUnknownUsageFile)

  lazy val generated = module.process(inFile,
    Config(packageNames = Map(None -> Some("ipo")),
      outdir = tmp,
      flags = Map("ignoreUnknown" -> true)
    ))

  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
      "Address(\"\", \"\", \"\").toString"),
      generated) must evaluateTo("Address(,,)", outdir = "./tmp")
  }

  "ipo.scala file must compile together with PurchaseOrderIgnoreUnknownUsage.scala" in {
    (List("import ipo._",
      "PurchaseOrderIgnoreUnknownUsage.allTests"),
      ignoreUnknownUsageFile :: generated) must evaluateTo(true, outdir = "./tmp")
  }

}