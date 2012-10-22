package scalaxb

trait DispatchHttpClients extends HttpClients {
  val httpClient = new DispatchHttpClient {}

  trait DispatchHttpClient extends HttpClient {
    import dispatch._

    def request(in: String, address: java.net.URI, headers: Map[String, String]): String = {
      val req = url(address.toString) << in <:< headers
      val s = Http(req OK as.String)
      s()
    }
  }
}
