package scalaxb

import scala.concurrent.{ Future, ExecutionContext }

trait HttpClientsAsync {
  def httpClient: HttpClient

  trait HttpClient {
    def request(in: String, address: java.net.URI, headers: Map[String, String])(implicit ec: ExecutionContext): Future[String]
  }
}
