import org.apache.cxf.endpoint.Server
import org.apache.cxf.jaxws.JaxWsServerFactoryBean
import org.apache.cxf.transport.http_jetty.JettyHTTPServerEngineFactory
import java.io.{ File, ByteArrayInputStream, FileOutputStream }

trait JaxwsTestBase {
  lazy val server = initServer
  def serviceImpl: Any
  def servicePort: Int = 8080
  def serviceAddress: String

  def writeStringToFile(content: String, file: File): Unit =
    sys.process.BasicIO.transferFully(new ByteArrayInputStream(content.getBytes("UTF-8")), new FileOutputStream(file))
  def retrieveWsdl: String = {
    import scala.concurrent._, duration._
    import gigahorse._, support.okhttp.Gigahorse
    val http = Gigahorse.http(Gigahorse.config)
    val wsdl = Gigahorse.url(s"http://localhost:$servicePort/$serviceAddress?wsdl").get
    val wsdlAsStr = http.run(wsdl, Gigahorse.asString)
    Await.result(wsdlAsStr, 10.seconds)
  }

  def initServer: Server = {
    val svrFactory = new JaxWsServerFactoryBean
    svrFactory.setServiceClass(serviceImpl.getClass)
    svrFactory.setAddress(s"http://localhost:$servicePort/$serviceAddress")
    svrFactory.setServiceBean(serviceImpl)
    svrFactory.create    
  }

  def startServer: Server = server

  def stopServer: Unit = {
    server.stop
    val jettyFactory = new JettyHTTPServerEngineFactory
    jettyFactory.retrieveJettyHTTPServerEngine(servicePort).shutdown 
  }
}
