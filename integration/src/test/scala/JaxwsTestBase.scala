package scalaxb.specs

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
    import dispatch._, Defaults._
    val wsdl = url(s"http://localhost:$servicePort/$serviceAddress?wsdl")
    val wsdlAsStr = Http(wsdl OK as.String)
    wsdlAsStr()    
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
