import org.apache.cxf.endpoint.Server
import org.apache.cxf.jaxrs.JAXRSServerFactoryBean
import org.apache.cxf.transport.http_jetty.JettyHTTPServerEngineFactory

trait JaxrsTestBase {
  lazy val server = initServer
  def serviceImpl: Any
  def servicePort: Int = 8080
  def serviceAddress: String

  def initServer: Server = {
    val svrFactory = new JAXRSServerFactoryBean
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
