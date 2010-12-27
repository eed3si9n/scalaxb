package scalaxb.servlet.model

import java.net.{URI}
import java.io.{InputStream, OutputStream, ByteArrayOutputStream, ByteArrayInputStream}

case class ScalaFile(fileName: String, content: String) {
  def inputStream: InputStream =
    new ByteArrayInputStream(content.getBytes)
  
  def write(out: OutputStream) {
    val in = inputStream
    try {
      val buffer = new Array[Byte](1024)
      Iterator.continually(in.read(buffer))
        .takeWhile(_ != -1)
        .foreach { out.write(buffer, 0 , _) }
    }
    finally {
      in.close
    }    
  }
}

object ScalaFile {
  import java.util.zip.{ZipOutputStream, ZipEntry}
  
  def fromURI(url: URI, content: String) = {
    val xsdFileName = url.getPath.split("/").toList.reverse.head
    val fileName = """([.]\w+)$""".r.replaceFirstIn(xsdFileName, ".scala")
    ScalaFile(fileName, content)
  }
  
  def fromResource(source: String, fileName: String) = {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.StringWriter()
    val printout = new java.io.PrintWriter(out)
    var line: Option[String] = None
    line = Option[String](reader.readLine)
    while (line != None) {
      line foreach { printout.println }
      line = Option[String](reader.readLine)
    }
    in.close
    printout.flush
    ScalaFile(fileName, out.toString)
  }
  
  def zip(files: Seq[ScalaFile]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val zout = new ZipOutputStream(out)
    files map { file =>
      zout.putNextEntry(new ZipEntry(file.fileName))
      file.write(zout)
    }
    zout.close
    out.toByteArray
  }
}
