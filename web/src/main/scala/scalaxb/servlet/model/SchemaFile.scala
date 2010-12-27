package scalaxb.servlet.model

import java.net.{URI}
import java.io.{File, Reader, StringWriter, PrintWriter}

case class SchemaFile(uri: URI, reader: Reader) {
  val out = new StringWriter
  val printout = new PrintWriter(out)
  
  def toTriplet = (uri, reader, printout)
}

object SchemaFile {
  import java.net.URL
  
  def fromURI(uri: URI): SchemaFile = {
    import com.google.appengine.api.urlfetch._

    val svc = URLFetchServiceFactory.getURLFetchService();
    val response = svc.fetch(uri.toURL)
    val stream = new java.io.ByteArrayInputStream(response.getContent)
    val reader = new java.io.InputStreamReader(stream)
    SchemaFile(uri, reader)
  }
  
  def fromBytes(name: String, content: Array[Byte]) = {
    val stream = new java.io.ByteArrayInputStream(content)
    val reader = new java.io.InputStreamReader(stream)
    SchemaFile(new File(name).toURI, reader)    
  }
}
