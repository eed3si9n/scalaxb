import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.xsd.Driver

object DispatchResponseAsScalaxbTest extends TestBase with JaxrsTestBase {
  override val module = new Driver // with Verbose

  def serviceImpl:RestService = new RestService()
  def serviceAddress: String = "dispatch-response-as-scalaxb"

  step {
    startServer
  }

  val packageName = "stockquote"
  val xsdFile = new File(s"integration/src/test/resources/item.xsd")
  lazy val generated = {
    module.process(xsdFile,
      Config(packageNames = Map(None -> Some(packageName)),
      packageDir = true, outdir = tmp, async = true))
  }

  "dispatch-response-as-scalaxb service works" in {
    (List("""import stockquote._
      import scala.concurrent._, duration._, dispatch._""",
      s"""val request = url("http://localhost:$servicePort/$serviceAddress/item/GOOG")""",
      """val fresponse = Http(request > as.scalaxb[StoreItem])""",
      """val response = Await.result(fresponse, 5 seconds)""",
      """if (response != StoreItem(symbol = "GOOG", price = 42.0)) sys.error(response.toString)""",

      """true"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  step {
    stopServer
  }
}