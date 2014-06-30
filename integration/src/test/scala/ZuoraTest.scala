import scalaxb.compiler.wsdl11.{Driver}
import java.io.{File}
import scalaxb.compiler.{Config}

object ZuoraTest extends TestBase {
  override val module = new Driver // with Verbose

  lazy val generated = module.process(inFile,
    Config(packageNames = Map(None -> Some(packageName)),
      packageDir = true, outdir = tmp, async = true))

  val packageName = "zuora"
  val inFile  = new File("integration/src/test/resources/zuora.wsdl")
  "zuora.scala file must compile" in {
    (List("""true"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
