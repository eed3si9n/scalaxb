import scalaxb.compiler.wsdl11.{Driver}
import java.io.{File}
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object Wsdl11Soap11AsyncTest extends TestBase {
  override val module = new Driver // with Verbose

  val packageName = "genericbarcode"
  val inFile  = new File("integration/src/test/resources/genericbarcode.wsdl")
  val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GenerateGigahorseClient).
      update(GeneratePackageDir)
  lazy val generated = module.process(inFile, config)
  "stockquote.scala file must compile" in {
    (List("""import genericbarcode._
      import scala.concurrent._
      import scala.concurrent.duration._""",
      """val service = (new BarCodeSoapBindings with scalaxb.Soap11ClientsAsync with scalaxb.GlobalExecutionContextProvider with scalaxb.DispatchHttpClientsAsync {}).service
       val data = BarCodeData(120, 120, 0, 1, 1, 20, 20, true, None, None, None, 10.0f, Both, CodeEAN128B, NoneType, BottomCenter, PNG)
       println(scalaxb.toXML(data, "BarCodeParam", defaultScope))
       val fresponse = service.generateBarCode(data, Some("1234"))
       val response = Await.result(fresponse, 5.seconds)
       println(response)""",
       """response.toString.contains("iVB")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
  "stockquote.scala file must compile with Gigahorse" in {
    (List("""import genericbarcode._
      import scala.concurrent._
      import scala.concurrent.duration._""",
      """val service = (new BarCodeSoapBindings with scalaxb.Soap11ClientsAsync with scalaxb.GlobalExecutionContextProvider with scalaxb.GigahorseHttpClientsAsync {}).service
       val data = BarCodeData(120, 120, 0, 1, 1, 20, 20, true, None, None, None, 10.0f, Both, CodeEAN128B, NoneType, BottomCenter, PNG)
       println(scalaxb.toXML(data, "BarCodeParam", defaultScope))
       val fresponse = service.generateBarCode(data, Some("1234"))
       val response = Await.result(fresponse, 5.seconds)
       println(response)""",
      """response.toString.contains("iVB")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
