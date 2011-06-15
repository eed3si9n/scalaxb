import scalaxb.compiler.{Verbose}
import scalaxb.compiler.wsdl11.{Driver}
import java.io.{File}
import scalaxb.compiler.{Config}

object Wsdl11Test extends TestBase {
  override val module = new Driver with Verbose
  val inFile  = new File("integration/src/test/resources/stockquote.wsdl")

  lazy val generated = module.process(inFile,
    Config(packageNames = Map(None -> Some("stockquote")),
      packageDir = true, outdir = tmp))
  val dependencyPath = new File("integration/lib_managed/scala_2.9.0/test/").getAbsoluteFile.listFiles.toList
  "stockquote.scala file must compile" in {
    (List("""val service = (new stockquote.StockQuoteSoap12s with scalaxb.SoapClients with scalaxb.DispatchHttpClients {}).service
       val response = service.getQuote(Some("GOOG"))""",
       """response.toString.contains("<Symbol>GOOG</Symbol>")"""), generated) must evaluateTo(true,
      outdir = "./tmp", classpath = dependencyPath map {_.toString})
  }
}
