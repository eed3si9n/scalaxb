package scalaxb

import scala.concurrent._, duration._

trait DispatchHttpClientsTimeout extends HttpClientsAsync {
  lazy val httpClient = new DispatchHttpClient {}
  def requestTimeout: Duration = 10 seconds
  def connectionTimeout: Duration = 10 seconds

  trait DispatchHttpClient extends HttpClient {
    import dispatch._, Defaults._

    // Keep it lazy. See https://github.com/eed3si9n/scalaxb/pull/279
    lazy val http = Http.configure(_.
      setRequestTimeoutInMs(requestTimeout.toMillis.toInt).
      setConnectionTimeoutInMs(connectionTimeout.toMillis.toInt))
    
    def request(in: String, address: java.net.URI, headers: Map[String, String]): concurrent.Future[String] = {
      val req = url(address.toString).setBodyEncoding("UTF-8") <:< headers << in
      http(req > as.String)
    }
  }
}
