import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._
import scalaxb.compiler.wsdl11.Driver
import scalaxb.stockquote.server._

class Http4sResponseAsScalaxbTest extends TestBase with JaxwsTestBase {
  override val module = new Driver // with Verbose

  def serviceImpl = new DocumentLiteralBareService()
  def serviceAddress: String = "document-bare"

  step {
    startServer
  }

  val packageName = "stockquote"
  val wsdlFile11 = new File(s"integration/target/$serviceAddress.wsdl") //new File(s"integration/src/test/resources/stockquote-11.wsdl")
  val wsdlFile12 = new File(s"integration/src/test/resources/stockquote.wsdl")
  val config = Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir).
      update(HttpClientStyle.Tagless).
      update(GenerateHttp4sClient).
      update(GenerateMapK).
      remove(GenerateDispatchClient)
  def generated11 = {
    writeStringToFile(retrieveWsdl, wsdlFile11)
    module.process(wsdlFile11, config)
  }
  def generated12 = {
    module.process(wsdlFile12, config)
  }

  "http4s-response-as-scalaxb service works soap 1.1" in {
    (List("""import stockquote._;import org.http4s.implicits._;import org.http4s.ember.client._;import cats.effect._;""",
      "import cats.effect.unsafe.implicits.global;",
      """def makeClient(emberClient: org.http4s.client.Client[IO]) = {
           new DocumentLiteralBareServiceSoapBindings[IO]
             with scalaxb.Soap11ClientsF[IO]
             with scalaxb.Http4sClientsF[IO] {
               override protected def F: Concurrent[IO] = IO.asyncForIO
               override protected def http4sClient: org.http4s.client.Client[IO] = emberClient
           }.service
      }""",
      """def build = EmberClientBuilder.default[IO].build.use { emberClient =>
           val client = makeClient(emberClient)
           client.price(Option("JHA"))
      }""",
      "build.unsafeRunSync() == Some(42)"
      ), generated11) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true, feature = false)
  }

  "http4s-response-as-scalaxb service compiles soap 1.2" in {
    (List("""import stockquote._;import org.http4s.implicits._;import cats.effect._;""",
      """def makeClient(emberClient: org.http4s.client.Client[IO]) = {
           new StockQuoteSoap12Bindings[IO]
             with scalaxb.SoapClientsF[IO]
             with scalaxb.Http4sClientsF[IO] {
               override protected def F: Concurrent[IO] = IO.asyncForIO
               override protected def http4sClient: org.http4s.client.Client[IO] = emberClient
           }.service
       }""",
       """true"""), generated12) must evaluateTo(true,
       outdir = "./tmp", usecurrentcp = true, feature = false)
   }

  step {
    stopServer
  }
}
