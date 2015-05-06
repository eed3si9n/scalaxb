import java.io.{File}
import org.specs2.matcher.ValueCheck.valueIsTypedValueCheck

import scalaxb.compiler.Config

object LensPurchaseOrderTest extends TestBase {
  val inFile    = new File("integration/src/test/resources/ipo.xsd")
  val usageFile = new File(tmp, "PurchaseOrderUsage.scala")
  copyFileFromResource("PurchaseOrderUsage.scala", usageFile)

  // override val module = new scalaxb.compiler.xsd.Driver with Verbose
  lazy val generated = module.process(inFile,
      new Config(packageNames = Map(None -> Some("ipo")),
      outdir = tmp,
      flags = Map("generateLens" -> true)
    ))

  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
          "Address.name.set(Address(\"\", \"\", \"\"), \"hello\").toString"),
     generated) must evaluateTo("Address(hello,,)", outdir = "./tmp", usecurrentcp = true)
  }
  
  "ipo.scala file must compile together with PurchaseOrderUsage.scala" in {
    (List("import ipo._",
          "PurchaseOrderUsage.allTests"),
     usageFile :: generated) must evaluateTo(true, outdir = "./tmp", usecurrentcp = true)
  }
}
