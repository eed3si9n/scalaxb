package scalaxb

import concurrent.Future

trait HttpClientsAsync {
  def httpClient: HttpClient

  trait HttpClient {
    def request(in: String, address: java.net.URI, headers: Map[String, String]): Future[String]
  }
}
