import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object Wsdl11Soap11Test extends TestBase {
  override val module = new Driver // with Verbose

  lazy val generated = module.process(inFile, config)

  val packageName = "genericbarcode"
  val inFile  = new File("integration/src/test/resources/genericbarcode.wsdl")
  val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GenerateGigahorseClient).
      update(GeneratePackageDir).
      remove(GenerateAsync)
  "stockquote.scala file must compile" in {
    (List("""import genericbarcode._""",
       """val service = (new BarCodeSoapBindings with scalaxb.Soap11Clients with scalaxb.DispatchHttpClients {}).service
       val data = BarCodeData(120, 120, 0, 1, 1, 20, 20, true, None, None, None, 10.0f, Both, CodeEAN128B, NoneType, BottomCenter, PNG)
       println(scalaxb.toXML(data, "BarCodeParam", defaultScope))
       val response = service.generateBarCode(data, Some("1234"))""",
       """response.right.get.toString.contains("iVB")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
  "stockquote.scala file must compile with Gigahorse" in {
    (List("""import genericbarcode._""",
      """val service = (new BarCodeSoapBindings with scalaxb.Soap11Clients with scalaxb.GigahorseHttpClients {}).service
       val data = BarCodeData(120, 120, 0, 1, 1, 20, 20, true, None, None, None, 10.0f, Both, CodeEAN128B, NoneType, BottomCenter, PNG)
       println(scalaxb.toXML(data, "BarCodeParam", defaultScope))
       val response = service.generateBarCode(data, Some("1234"))""",
      """response.right.get.toString.contains("iVB")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
