import java.io.{File}

object PurchaseOrderTest extends TestBase {
  val inFile    = new File("src/test/resources/ipo.xsd")
  val outFile   = new File(tmp, "ipo.scala")
  val usageFile = new File(tmp, "PurchaseOrderUsage.scala")
  copyFileFromResource("PurchaseOrderUsage.scala", usageFile)
  
  "ipo.xsd is parsable" in {
     module.parse(inFile) must be like {
       case schema: org.scalaxb.compiler.xsd.SchemaDecl => true
     }
  }
  
  lazy val generated = module.process(inFile, outFile, "ipo")
  
  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
          "Address(\"\", \"\", \"\").toString"), 
     generated) must evaluateTo("Address(,,)", outdir = "./tmp")
  }
  
  "ipo.scala file must compile together with PurchaseOrderUsage.scala" in {
    (List("import ipo._",
          "PurchaseOrderUsage.allTests"),
     usageFile :: generated) must evaluateTo(true, outdir = "./tmp")
  }
}
