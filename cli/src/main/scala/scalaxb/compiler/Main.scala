/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler

import scalaxb.{Version}
import scopt.OptionParser
import scala.collection.{Map, Set}
import scala.collection.mutable.{ListBuffer, ListMap}
import java.io.{File, BufferedReader, Reader, PrintWriter}

object Main extends Version {
  def main(args: Array[String]) {
    try { start(args) } 
    catch {
      case e: ReferenceNotFound =>
        println(e.getMessage)
      case e: CaseClassTooLong =>
        println(e.getMessage)
      case e: Exception =>
        e.printStackTrace
    }
  }
  
  def start(args: Seq[String]) {
    var verbose = false
    val packageNames: ListMap[Option[String], Option[String]] =
      ListMap.empty[Option[String], Option[String]]
    val wrappedComplexTypes: ListBuffer[String] = ListBuffer.empty[String]
    var outdir: File = new File(".")
    var classPrefix: Option[String] = None
    var paramPrefix: Option[String] = None
    val files = ListBuffer.empty[File]
    var packageDir = false

    packageNames(None) = None
    val paramParser = new OptionParser("scalaxb", version) {
      opt("d", "outdir", "<directory>", "generated files will go into <directory>",
        { d: String => outdir = new File(d) })
      opt("p", "package", "<package>", "specifies the target package",
        { p: String => packageNames(None) = Some(p) })
      keyValueOpt("p", "package", "<namespaceURI>", "<package>",
        "specifies the target package for <namespaceURI>",
        { (key: String, value: String) => { packageNames(Some(key)) = Some(value) } })
      opt(None, "class-prefix", "<prefix>", "prefixes generated class names",
        { p: String => classPrefix = Some(p) })
      opt(None, "param-prefix", "<prefix>", "prefixes generated parameter names",
        { p: String => paramPrefix = Some(p) })
      opt(None, "wrap-contents", "<complexType>",
        "wraps inner contents into a seperate case class",
        { w: String => wrappedComplexTypes append w })
      opt("package-dir", "generate package directories",
        { packageDir = true })
      opt("v", "verbose", "be extra verbose",
        { verbose = true })
      arglist("<schema_file>...", "input schema to be converted",
        { x: String => files append (new File(x)) })
    }

    if (paramParser.parse(args) && !files.isEmpty) {
      val module = Module.moduleByFileName(files.head, verbose)

      module.processFiles(files,
        Config(packageNames = packageNames,
          outdir = outdir,
          packageDir = packageDir,
          classPrefix = classPrefix,
          paramPrefix = paramPrefix,
          wrappedComplexTypes = wrappedComplexTypes.toList) )
    }
  }
}
