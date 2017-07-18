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
import ConfigEntry._

object Defaults {
  val opOutputWrapperPostfix = "Output"
  val protocolFileName = "xmlprotocol.scala"
}

object Main {
  private val log = Log.forName("main")

  def main(args: Array[String]): Unit = {
    // change this change SbtApp too.
    try { start(args); }
    catch {
      case e: ReferenceNotFound =>
        log.error(e.getMessage)
      case e: CaseClassTooLong =>
        log.error(e.getMessage)
      case e: Exception =>
        log.error(e.getStackTrace.mkString("", Module.NL, Module.NL))
    }
  }

  def start(args: Seq[String]) {
    Arguments(args) foreach { args =>
      Log.configureLogger(args.verbose)
      val module = Module.moduleByFileName(args.files.head)
      module.processFiles(args.files.toList, args.config)
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
    val files = ListBuffer.empty[File]

    val paramParser = new scopt.OptionParser[Config]("scalaxb") {
      head("scalaxb", scalaxb.BuildInfo.version)
      opt[File]('d', "outdir") valueName("<directory>") text("generated files will go into <directory>") action { (x, c) =>
        c.update(Outdir(x)) }
      opt[String]('p', "default-package") valueName("<package>") text("specifies the target package") action { (x, c) =>
        c.update(PackageNames(c.packageNames updated (None, Some(x)))) }
      opt[(String, String)]("package") unbounded() keyValueName("<namespaceURI>", "<package>") text(
        "specifies the target package for <namespaceURI>") action { case ((k, v), c) =>
        c.update(PackageNames(c.packageNames updated (Some(k), Some(v)))) }
      opt[Unit]("autopackages") text("generates packages for different namespaces automatically") action { (_,c) =>
        c.update(AutoPackages) }
      opt[String]("class-prefix") valueName("<prefix>") text("prefixes generated class names") action { (x, c) =>
        c.update(ClassPrefix(x)) }
      opt[String]("param-prefix") valueName("<prefix>") text("prefixes generated parameter names") action { (x, c) =>
        c.update(ParamPrefix(x)) }
      opt[String]("attribute-prefix") valueName("<prefix>") text("prefixes generated attribute parameters") action { (x, c) =>
        c.update(AttributePrefix(x)) }
      opt[String]("op-output-wrapper-postfix") valueName("<postfix>") text("postfixes operation output wrapper names (default: Output)") action { (x, c) =>
        c.update(OpOutputWrapperPostfix(x)) }
      opt[Unit]("prepend-family") text("prepends family name to class names") action { (_, c) =>
        c.update(PrependFamilyName) }
      opt[String]("wrap-contents") valueName("<complexType>") text("wraps inner contents into a seperate case class") action { (x, c) =>
        c.update(WrappedComplexTypes(c.wrappedComplexTypes :+ x)) }
      opt[Int]("contents-limit") valueName("<size>") text("defines long contents to be segmented (default: max)") action { (x, c) =>
        c.update(ContentsSizeLimit(x)) }
      opt[Int]("chunk-size") valueName("<size>") text("segments long sequences into chunks (default: 10)") action { (x, c) =>
        c.update(SequenceChunkSize(x)) }
      opt[Unit]("named-attributes") text("generates named fields for attributes") action { (_, c) =>
        c.update(NamedAttributes) }
      opt[Unit]("package-dir") text("generates package directories") action { (_, c) =>
        c.update(GeneratePackageDir) }
      opt[String]("protocol-file") valueName("<name.scala>") text("protocol file name (xmlprotocol.scala)") action { (x, c) =>
        c.update(ProtocolFileName(x)) }
      opt[String]("protocol-package") valueName("<package>") text("package for protocols") action { (x, c) =>
        c.update(ProtocolPackageName(Some(x))) }
      opt[Unit]("no-runtime") text("skips runtime files") action { (_, c) =>
        c.remove(GenerateRuntime) }
      opt[Unit]("no-dispatch-client") text("disables generation of Dispatch client") action { (_, c) =>
        c.remove(GenerateDispatchClient) }
      opt[Unit]("dispatch-as") text("generates Dispatch \"as\"") action { (_, c) =>
        c.update(GenerateDispatchAs) }
      opt[Unit]("mutable") text("generates mutable classes") action { (_,c) =>
        c.update(GenerateMutable).remove(VarArg) }
      opt[Unit]("visitor") text("generates visitor") action { (_,c) =>
        c.update(GenerateVisitor) }
      opt[Unit]("lax-any") text("relaxes namespace constraints of xs:any") action { (_, c) =>
        c.update(LaxAny) }
      opt[Unit]("blocking") text("generates blocking SOAP client") action { (_, c) =>
        c.remove(GenerateAsync) }
      opt[String]("dispatch-version") valueName("<version>") text("version of Dispatch (default: " + scalaxb.BuildInfo.defaultDispatchVersion + ")") action { (x, c) =>
        c.update(DispatchVersion(x)) }
      opt[Unit]("no-varargs") text("uses Seq instead of the varargs") action { (_, c) =>
        c.remove(VarArg) }
      opt[Unit]("ignore-unknown") text("ignores unknown Elements") action { (_, c) =>
        c.update(IgnoreUnknown) }

      opt[Unit]('v', "verbose") text("be extra verbose") action { (_, c) =>
        verbose = true
        c
      }

      help("help") text("display this message")
      version("version") text("display version info")
      arg[File]("<schema_file>...") unbounded() text("input schema to be converted") action { (x, c) =>
        files append x
        c
      }
    }
    paramParser.parse(args, Config.default) flatMap { c =>
      if (files.isEmpty) None
      else Some(Arguments(c, files, verbose))
    }
  }
}
