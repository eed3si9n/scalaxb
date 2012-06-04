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

import scala.collection.mutable.{ListBuffer, ListMap}
import java.io.File

object Defaults {
  val protocolFileName = "xmlprotocol.scala"
}

object Main {
  private val log = Log.forName("main")

  def main(args: Array[String]) {
    // change this change SbtApp too.
    try { start(args); }
    catch {
      case e: ReferenceNotFound =>
        log.error(e.getMessage)
      case e: CaseClassTooLong =>
        log.error(e.getMessage)
      case e: Exception =>
        log.error(e.getStackTraceString)
    }
  }

  def start(args: Seq[String]) {
    Arguments(args) foreach { args =>
      Log.configureLogger(args.verbose)
      val module = Module.moduleByFileName(args.files.head)
      module.processFiles(args.files, args.config)
    }
  }

}

/**
 * The parsed command line arguments.
 */
case class Arguments(
      config: Config,
      files: Seq[File],
      verbose: Boolean)

object Arguments {

  /**
   * Parse the command line arguments. The result is an option, and is None if
   * the command line arguments were invalid, or if no files were specified.
   */
  def apply(args: Seq[String]): Option[Arguments] = {
    var verbose = false
    val packageNames: ListMap[Option[String], Option[String]] =
      ListMap.empty[Option[String], Option[String]]
    val wrappedComplexTypes: ListBuffer[String] = ListBuffer.empty[String]
    var outdir: File = new File(".")
    var classPrefix: Option[String] = None
    var paramPrefix: Option[String] = None
    var attributePrefix: Option[String] = None
    val files = ListBuffer.empty[File]
    var packageDir = false
    var protocolFileName = Defaults.protocolFileName
    var protocolPackageName: Option[String] = None
    var generateRuntime = true
    var sequenceChunkSize = 10
    var contentsSizeLimit = 20
    var prependFamilyName = false
    var laxAny = false

    packageNames(None) = None
    val paramParser = new scopt.OptionParser("scalaxb", scalaxb.BuildInfo.version) {
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
      opt(None, "attribute-prefix", "<prefix>", "prefixes generated attribute parameters",
        { p: String => attributePrefix = Some(p) })
      opt("prepend-family", "prepends family name to class names",
        { prependFamilyName = true })
      opt(None, "wrap-contents", "<complexType>",
        "wraps inner contents into a seperate case class",
        { w: String => wrappedComplexTypes append w })
      intOpt(None, "contents-limit", "<size>", "defines long contents to be segmented (default: 20)",
        { size: Int => contentsSizeLimit = size })
      intOpt(None, "chunk-size", "<size>", "segments long sequences into chunks (default: 10)",
        { size: Int => sequenceChunkSize = size })
      opt("package-dir", "generates package directories",
        { packageDir = true })
      opt(None, "protocol-file", "<name.scala>", "protocol file name (xmlprotocol.scala)",
        { p: String => protocolFileName = p})
      opt(None, "protocol-package", "<package>", "package for protocols",
        { p: String => protocolPackageName = Some(p)})
      opt("no-runtime", "skips runtime files",
        { generateRuntime = false })
      opt("lax-any", "relaxes namespace constraints of xs:any",
        { laxAny = true })
      opt("v", "verbose", "be extra verbose",
        { verbose = true })
      help(None, "version", "display this message")
      arglist("<schema_file>...", "input schema to be converted",
        { x: String => files append (new File(x)) })
    }

    if (paramParser.parse(args) && !files.isEmpty) {
        val config = Config(packageNames = packageNames,
          outdir = outdir,
          packageDir = packageDir,
          classPrefix = classPrefix,
          paramPrefix = paramPrefix,
          attributePrefix = attributePrefix,
          wrappedComplexTypes = wrappedComplexTypes.toList,
          protocolFileName = protocolFileName,
          protocolPackageName = protocolPackageName,
          generateRuntime = generateRuntime,
          contentsSizeLimit = contentsSizeLimit,
          sequenceChunkSize = sequenceChunkSize,
          prependFamilyName = prependFamilyName,
          laxAny = laxAny)
        Some(Arguments(config, files, verbose))
    } else {
      None
    }
  }

}
