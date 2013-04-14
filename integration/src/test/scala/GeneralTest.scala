package scalaxb.specs

import java.io.{File}
import scalaxb.compiler._

object GeneralTest extends TestBase {
  import scalaxb.compiler.xsd.Driver
  import scalaxb.compiler.xsd2.{Driver => Driver2}

  Log.configureLogger(true)
  override val module: Module = new Driver2
  
  val inFile    = new File("integration/src/test/resources/general.xsd")
  val importFile = new File("integration/src/test/resources/general_import.xsd")
  val mimeFile  = new File("integration/src/test/resources/xmlmime.xsd")
  val usageFile = new File(tmp, "GeneralUsage.scala")
  val custumFile = new File(tmp, "CustomizationUsage.scala")
  
  lazy val generated = module.processFiles(Seq(inFile, mimeFile, importFile),
    Config(packageNames = Map(None -> Some("general"),
        Some("http://www.w3.org/2005/05/xmlmime") -> Some("xmlmime"),
        Some("http://www.example.com/general_import") -> Some("gimport")),
      attributePrefix = Some("attr_"),
      outdir = tmp))
  ResourceUtil.copyFileFromResource("GeneralUsage.scala", usageFile)
  ResourceUtil.copyFileFromResource("CustomizationUsage.scala", custumFile)
    
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
