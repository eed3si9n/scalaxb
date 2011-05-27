package scalaxb

trait DispatchHttpClients extends HttpClients {
  val httpClient = new DispatchHttpClient {}

  trait DispatchHttpClient extends HttpClient {
    import dispatch._

    def request(in: String, address: java.net.URI, action: Option[java.net.URI]): String = {
      val http = new Http
      val header = Map(action.toList map { x => ("SOAPAction", "\"%s\"".format(x.toString)) }: _*)
      http(url(address.toString) << (in, "application/soap+xml") <:< header as_str)
    }
  }
}
