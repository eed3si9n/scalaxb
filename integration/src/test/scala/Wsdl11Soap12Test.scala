import scalaxb.compiler.wsdl11.Driver
import java.io.File
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object Wsdl11Soap12Test extends TestBase {
  override val module = new Driver // with Verbose

  "stockquote.scala file must compile" in {

    val packageName = "stockquote"
    val inFile  = new File("integration/src/test/resources/stockquote.wsdl")
    val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir).
      remove(GenerateAsync)
    lazy val generated = module.process(inFile, config)

    // fixme: temp fix - getquote service returns always "exception"
    (List(
      "import stockquote.XMLProtocol._",
      """val service = (new StockQuoteSoap12Bindings with scalaxb.SoapClients with scalaxb.DispatchHttpClients {}).service
       val response = service.getQuote(Some("GOOG"))""",
       """response.toString.contains("exception")"""), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  "implicitheader.scala file must compile" in {

    val packageName = "implicitheader"
    val inFile  = new File("integration/src/test/resources/implicit_header_example.wsdl")
    val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir).
      remove(GenerateAsync)
    lazy val generated = module.process(inFile, config)

    (List(
      "import implicitheader.XMLProtocol._",
      """
        |val service = new UserBindings with scalaxb.SoapClients with scalaxb.HttpClients {
        |      override def httpClient = new HttpClient {
        |        override def request(in: String, address: java.net.URI, headers: Map[String, String]): String = {
        |          val expectedReq =
        |            <soap12:Envelope xmlns:soap12="http://www.w3.org/2003/05/soap-envelope"
        |                             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |                             xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |                             xmlns:tns="http://tempuri.org/"
        |                             xmlns="http://tempuri.org/">
        |              <soap12:Header>header</soap12:Header>
        |              <soap12:Body>User</soap12:Body>
        |            </soap12:Envelope>
        |          val currentReq = scala.xml.XML.loadString(in)
        |          if(scala.xml.Utility.trim(currentReq) != scala.xml.Utility.trim(expectedReq) ) {
        |            throw new Exception("invalid request")
        |          }
        |
        |          val xml =
        |            <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope"
        |                           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |                           xmlns:xsd="http://www.w3.org/2001/XMLSchema">
        |              <soap:Body>
        |                <QuoteResponse xmlns="http://tempuri.org/">
        |                  <Message>User created</Message>
        |                </QuoteResponse>
        |              </soap:Body>
        |            </soap:Envelope>
        |          xml.toString()
        |        }
        |      }
        |
        |      override def baseAddress: java.net.URI = new java.net.URI("mock")
        |
        |    }.service
        |    service.createUser("User", "header") match {
        |      case Right(implicitheader.QuoteResponse(Some("User created"))) => true
        |      case _ => false
        |    }
      """.stripMargin), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }

  "explicitheader.scala file must compile" in {

    val packageName = "explicitheader"
    val inFile  = new File("integration/src/test/resources/explicit_header_example.wsdl")
    val config =  Config.default.update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(tmp)).
      update(GeneratePackageDir).
      remove(GenerateAsync)
    lazy val generated = module.process(inFile, config)

    (List(
      "import explicitheader.XMLProtocol._",
      """
        |val service = new UserBindings with scalaxb.SoapClients with scalaxb.HttpClients {
        |      override def httpClient = new HttpClient {
        |        override def request(in: String, address: java.net.URI, headers: Map[String, String]): String = {
        |          val expectedReq =
        |            <soap12:Envelope xmlns:soap12="http://www.w3.org/2003/05/soap-envelope"
        |                             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |                             xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |                             xmlns:tns="http://tempuri.org/"
        |                             xmlns="http://tempuri.org/">
        |              <soap12:Header>header</soap12:Header>
        |              <soap12:Body>User</soap12:Body>
        |            </soap12:Envelope>
        |          val currentReq = scala.xml.XML.loadString(in)
        |          if(scala.xml.Utility.trim(currentReq) != scala.xml.Utility.trim(expectedReq) ) {
        |            throw new Exception("invalid request")
        |          }
        |
        |          val xml =
        |            <soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope"
        |                           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |                           xmlns:xsd="http://www.w3.org/2001/XMLSchema">
        |              <soap:Body>
        |                <QuoteResponse xmlns="http://tempuri.org/">
        |                  <Message>User created</Message>
        |                </QuoteResponse>
        |              </soap:Body>
        |            </soap:Envelope>
        |          xml.toString()
        |        }
        |      }
        |
        |      override def baseAddress: java.net.URI = new java.net.URI("mock")
        |
        |    }.service
        |    service.createUser("User", "header") match {
        |      case Right(explicitheader.QuoteResponse(Some("User created"))) => true
        |      case _ => false
        |    }
      """.stripMargin), generated) must evaluateTo(true,
      outdir = "./tmp", usecurrentcp = true)
  }
}
