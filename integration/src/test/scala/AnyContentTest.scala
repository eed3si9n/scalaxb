package scalaxb.specs

import java.io.{File}

object AnyContentTest extends TestBase {
  val inFile = new File("integration/src/test/resources/any.xsd")
  lazy val generated = module.process(inFile, "anycontent", tmp)
  val usageFile = new File(tmp, "AnyUsage.scala")
  ResourceUtil.copyFileFromResource("AnyUsage.scala", usageFile)
  
  "any.scala file must compile together with AnyUsage.scala" in {
    (List("AnyUsage.allTests"),
      usageFile :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
