import scalaxb.compiler.wsdl11.{Driver}
import java.io.{File}
import scalaxb.compiler.{Config}

object Wsdl11Soap11Test extends TestBase {
  override val module = new Driver // with Verbose

  lazy val generated = module.process(inFile,
    Config(packageNames = Map(None -> Some(packageName)),
      packageDir = true, outdir = tmp))

  val packageName = "genericbarcode"
  val inFile  = new File("integration/src/test/resources/genericbarcode.wsdl")
  "stockquote.scala file must compile" in {
    (List("""import genericbarcode._""",
       """val service = (new BarCodeSoapBindings with scalaxb.Soap11Clients with scalaxb.DispatchHttpClients {}).service
       val data = BarCodeData(120, 120, 0, 1, 1, 20, 20, true, None, None, None, 10.0f, Both, CodeEAN128B, NoneType, BottomCenter, PNG)
       println(scalaxb.toXML(data, "BarCodeParam", defaultScope))
       val response = service.generateBarCode(data, Some("1234")).right.get
       println(response)""",
       """response.toString.contains("iVB")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
