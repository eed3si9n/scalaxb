import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.stockquote.server._

object Wsdl11DocumentBareTest extends TestBase with JaxwsTestBase {
  override val module = new Driver // with Verbose

  def serviceImpl:DocumentLiteralBareService = new DocumentLiteralBareService()
  def serviceAddress: String = "document-bare"

  step {
    startServer
  }

  val packageName = "stockquote"
  val wsdlFile = new File(s"integration/target/$serviceAddress.wsdl")
  lazy val generated = {
    writeStringToFile(retrieveWsdl, wsdlFile)
    module.process(wsdlFile,
      Config(packageNames = Map(None -> Some(packageName)),
      packageDir = true, outdir = tmp, async = true))
  }

  "document-bare service works" in {
    (List("""import stockquote._
      import scala.concurrent._, duration._""",
      """val service = (new DocumentLiteralBareServiceSoapBindings with scalaxb.Soap11ClientsAsync with scalaxb.DispatchHttpClientsAsync {}).service""",
      """val fresponse = service.price(Some("GOOG"))
      val response = Await.result(fresponse, 5 seconds)
      if (response != Some(42.0)) sys.error(response.toString)""",
      """response.toString.contains("42")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  step {
    stopServer
  }
}
