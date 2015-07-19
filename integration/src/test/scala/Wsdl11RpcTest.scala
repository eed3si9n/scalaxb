import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._
import scalaxb.stockquote.server._

object Wsdl11RpcTest extends TestBase with JaxwsTestBase {
  override val module = new Driver // with Verbose

  def serviceImpl:RpcLiteralService = new RpcLiteralService()
  def serviceAddress: String = "rpc-literal"

  step {
    startServer
  }

  val packageName = "stockquote"
  val wsdlFile = new File(s"integration/target/$serviceAddress.wsdl")
  val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir)
  lazy val generated = {
    writeStringToFile(retrieveWsdl, wsdlFile)
    module.process(wsdlFile, config)
  }

  "document-bare service works" in {
    (List("""import stockquote._
      import scala.concurrent._, duration._""",
      """val service = (new RpcLiteralServiceSoapBindings with scalaxb.Soap11ClientsAsync with scalaxb.DispatchHttpClientsAsync {}).service""",
      """val fresponse = service.price("GOOG")""",
      """val response = Await.result(fresponse, 5 seconds)""",
      """if (response != 42.0) sys.error(response.toString)""",
      
      """val fresponse2 = service.useHeader(Some("GOOG"))""",
      """val response2 = Await.result(fresponse2, 5 seconds)""",
      """if (response2 != UseHeaderOutput(Some(42))) sys.error(response2.toString)""",

      """true"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  step {
    stopServer
  }
}
