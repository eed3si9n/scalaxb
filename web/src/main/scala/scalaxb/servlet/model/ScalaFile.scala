package scalaxb.servlet.model

import java.net.{URI}
import java.io.{InputStream, OutputStream, ByteArrayOutputStream, ByteArrayInputStream, StringWriter, PrintWriter}

class ScalaFile(val fileName: String) {
  val out = new StringWriter
  val printout = new PrintWriter(out)
  def content = out.toString

  def inputStream: InputStream =
    new ByteArrayInputStream(out.toString.getBytes)
  
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
