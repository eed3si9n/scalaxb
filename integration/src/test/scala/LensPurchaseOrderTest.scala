import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object LensPurchaseOrderTest extends TestBase {
  val inFile    = new File("integration/src/test/resources/ipo.xsd")
  val usageFile = new File(tmp, "PurchaseOrderUsage.scala")
  copyFileFromResource("PurchaseOrderUsage.scala", usageFile)

  // override val module = new scalaxb.compiler.xsd.Driver with Verbose
  val config = Config.default
    .update(PackageNames(Map(None -> Some("ipo"))))
    .update(Outdir(tmp))
    .update(UseLists)
    .update(GenerateLens)
  lazy val generated = module.process(inFile, config)

  "ipo.scala file must compile so Address can be used" in {
    (List("""import ipo._""",
          """Address.name.set("hello")(Address("", "", "")).toString"""),
     generated) must evaluateTo("Address(hello,,)", outdir = "./tmp", usecurrentcp = true)
  }

  "ipo.scala file must compile so compositional lens can be used" in {
    (List("""import ipo._""",
          """val items = Items(Item("a", BigInt(0), BigDecimal(0)) :: Nil)""",
          """val po = PurchaseOrderType(Address("", "", ""), Address("", "", ""), None, items)""",
          """val po2 = PurchaseOrderType.items.item.set(Item("b", BigInt(0), BigDecimal(0)) :: Nil)(po)""",
          """po2.toString"""),
     generated) must evaluateTo("PurchaseOrderType(Address(,,),Address(,,),None,Items(List(Item(b,0,0,None,None,Map()))),Map())", outdir = "./tmp", usecurrentcp = true)
  }

  "ipo.scala file must compile together with PurchaseOrderUsage.scala" in {
    (List("import ipo._",
          "PurchaseOrderUsage.allTests"),
     usageFile :: generated) must evaluateTo(true, outdir = "./tmp", usecurrentcp = true)
  }
}
