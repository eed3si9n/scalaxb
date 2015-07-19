import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object Wsdl11Soap12AsyncTest extends TestBase {
  override val module = new Driver // with Verbose

  lazy val generated = module.process(inFile, config)

  val packageName = "stockquote"
  val inFile  = new File("integration/src/test/resources/stockquote.wsdl")
  val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir)
  "stockquote.scala file must compile" in {
    (List("""import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent._
      import scala.concurrent.duration._""",
      """val service = (new stockquote.StockQuoteSoap12Bindings with scalaxb.SoapClientsAsync with scalaxb.DispatchHttpClientsAsync {}).service
       val fresponse = service.getQuote(Some("GOOG"))
       val response = Await.result(fresponse, 5 seconds)""",
       """response.toString.contains("<Symbol>GOOG</Symbol>")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
