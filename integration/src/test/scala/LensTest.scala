import java.io.File
import scalaxb.compiler.Config


/**
 * Note: as no scalaz dependency is added to the project, we can only test if the file is generated
 */
object LensTest extends TestBase {
  // override val module = new Driver with Verbose
  val inFile  = new File("integration/src/test/resources/lens.xsd")
  lazy val generated = module.process(inFile,
    Config(packageNames = Map(Some("http://www.w3.org/2001/XMLSchema") -> Some("org.w3.xmlschema")),
      outdir = tmp,
      generateLens = true
    ))
  "lens.xsd must generate lens.scala file" in {
    (generated(0) must exist)
  }
}