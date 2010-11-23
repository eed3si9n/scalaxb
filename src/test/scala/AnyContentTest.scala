import java.io.{File}

object AnyContentTest extends TestBase {
  val inFile = new File("src/test/resources/any.xsd")
  val outFile = new File(tmp, "any.scala")
  lazy val generated = module.process(inFile, outFile, outProtocolFile, "anycontent")
  val usageFile = new File(tmp, "AnyUsage.scala")
  copyFileFromResource("AnyUsage.scala", usageFile)
  
  "any.scala file must compile together with AnyUsage.scala" in {
    (List("AnyUsage.allTests"),
      usageFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
