import java.io.{File}

object GeneralTest extends TestBase {
  val inFile    = new File("src/test/resources/general.xsd")
  val outFile   = new File(tmp, "general.scala")
  val usageFile = new File(tmp, "GeneralUsage.scala")
  
  lazy val generated = module.process(inFile, outFile, "general")
  copyFileFromResource("GeneralUsage.scala", usageFile)
    
  "mixed.scala file must compile together with GeneralUsage.scala" in {
    (List("GeneralUsage.allTests"),
      usageFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
