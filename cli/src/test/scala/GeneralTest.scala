import java.io.{File}

object GeneralTest extends TestBase {
  val inFile    = new File("cli/src/test/resources/general.xsd")
  val outFile   = new File(tmp, "general.scala")
  val usageFile = new File(tmp, "GeneralUsage.scala")
  val custumFile = new File(tmp, "CustomizationUsage.scala")
  
  lazy val generated = module.process(inFile, outFile, outProtocolFile, "general")
  copyFileFromResource("GeneralUsage.scala", usageFile)
  copyFileFromResource("CustomizationUsage.scala", custumFile)
  
  "general.scala file must compile together with GeneralUsage.scala" in {
    (List("GeneralUsage.allTests"),
      usageFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
  
  "general.scala file must compile together with CustomizationUsage.scala" in {
    (List("CustomizationUsage.allTests"),
      custumFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
