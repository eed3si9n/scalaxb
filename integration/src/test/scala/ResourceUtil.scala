package scalaxb.specs

import java.io.{File}

object ResourceUtil {
  def copyFileFromResource(source: String, dest: File) {
    val in = getClass.getResourceAsStream("/" + source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.PrintWriter(new java.io.FileWriter(dest))
    var line: String = null
    line = reader.readLine
    while (line != null) {
      out.println(line)
      line = reader.readLine
    }
    in.close
    out.flush
  }
}
