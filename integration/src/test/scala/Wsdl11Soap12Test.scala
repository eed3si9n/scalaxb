import scalaxb.compiler.wsdl11.{Driver}
import java.io.{File}
import scalaxb.compiler.{Config}

object Wsdl11Soap12Test extends TestBase {
  override val module = new Driver // with Verbose

  lazy val generated = module.process(inFile,
    new Config(packageNames = Map(None -> Some(packageName)),
      packageDir = true, outdir = tmp, async = false))

  val packageName = "stockquote"
  val inFile  = new File("integration/src/test/resources/stockquote.wsdl")
  "stockquote.scala file must compile" in {
    (List("""val service = (new stockquote.StockQuoteSoap12Bindings with scalaxb.SoapClients with scalaxb.DispatchHttpClients {}).service
       val response = service.getQuote(Some("GOOG"))""",
       """response.toString.contains("<Symbol>GOOG</Symbol>")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
