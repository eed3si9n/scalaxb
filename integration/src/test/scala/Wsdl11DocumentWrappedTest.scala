import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.stockquote.server._

object Wsdl11DocumentWrappedTest extends TestBase with JaxwsTestBase {
  override val module = new Driver // with Verbose

  def serviceImpl:DocumentWrappedService = new DocumentWrappedService()
  def serviceAddress: String = "document-wrapped"

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

  "document-wrapped service works" in {
    (List("""import stockquote._
      import scala.concurrent._, duration._""",
      """val service = (new DocumentWrappedServiceSoapBindings with scalaxb.Soap11ClientsAsync with scalaxb.DispatchHttpClientsAsync {}).service""",
      """val fresponse = service.price(Some("GOOG"))""",
      """val response = Await.result(fresponse, 5 seconds)""",
      """if (response != 42.0) sys.error(response.toString) else ()""",      
      """true"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  step {
    stopServer
  }
}
