import java.io.{File}

object MixedContentTest extends TestBase {
  val inFile    = new File("src/test/resources/mixed.xsd")
  val outFile   = new File(tmp, "mixed.scala")
  val usageFile = new File(tmp, "MixedUsage.scala")
  
  lazy val generated = module.process(inFile, outFile, "mixed")
  copyFileFromResource("MixedUsage.scala", usageFile)
    
  "mixed.scala file must compile together with MixedUsage.scala" in {
    (List("MixedUsage.allTests"),
      usageFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
