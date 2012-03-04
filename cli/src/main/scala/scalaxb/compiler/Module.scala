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
import java.net.{URI}
import scala.xml.{Node, Elem}
import scala.xml.factory.{XMLLoader}
import javax.xml.parsers.SAXParser
import java.io.{File, PrintWriter, Reader, BufferedReader}
import com.codahale.logula.Log
import collection.{mutable, Map, Set}

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  classPostfix: Option[String] = None,
  paramPrefix: Option[String] = None,
  attributePrefix: Option[String] = None,
  outdir: File = new File("."),
  packageDir: Boolean = false,
  wrappedComplexTypes: List[String] = Nil,
  prependFamilyName: Boolean = false,
  seperateProtocol: Boolean = true,
  protocolFileName: String = "xmlprotocol.scala",
  protocolPackageName: Option[String] = None,
  defaultNamespace: Option[String] = None,
  generateRuntime: Boolean = true,
  contentsSizeLimit: Int = 20,
  sequenceChunkSize: Int = 10,
  laxAny: Boolean = false)

object Snippet {
  def apply(definition: Node): Snippet = Snippet(definition, Nil, Nil, Nil)

  def apply(snippets: Snippet*): Snippet =
    Snippet(snippets flatMap { s => s.companion ++ s.definition},
      Nil,
      snippets flatMap {_.defaultFormats},
      snippets flatMap {_.implicitValue})
}

case class Snippet(definition: Seq[Node],
  companion: Seq[Node],
  defaultFormats: Seq[Node],
  implicitValue: Seq[Node])

trait CanBeWriter[A] {
 def toWriter(value: A): PrintWriter
 def newInstance(packageName: Option[String], fileName: String): A
}

trait CanBeRawSchema[A, B] {
  def toRawSchema(value: A): B
  def toURI(value: A): URI
}

object Module {
  import scala.util.matching.Regex

  val NL = System.getProperty("line.separator")
  val FileExtension = """.*([.]\w+)$""".r

  def moduleByFileName(file: File, verbose: Boolean): Module = {
    configureLogger(verbose)
    file.toString match {
      case FileExtension(".wsdl") =>
        new scalaxb.compiler.wsdl11.Driver
      case _ =>
        new scalaxb.compiler.xsd.Driver
    }
  }

  def splitTypeName(value: String, scope: scala.xml.NamespaceBinding) = scalaxb.Helper.splitQName(value, scope)

  def indent(n: Int) = "  " * n

  def camelCase(name: String): String = {
    val (cap, rest) = name span {_.isUpper}
    cap.size match {
      case x if (x == 0) || (x == 1) || (x == name.size) => cap.toLowerCase + rest
      case x => (cap take (x - 1)).toLowerCase + (cap drop (x - 1)) + rest
    }
  }

  def configureLogger(verbose: Boolean) {
    import com.codahale.logula.Logging
    import org.apache.log4j.Level

    Logging.configure { log =>
      log.level = if (verbose) Level.TRACE
                  else Level.INFO
      log.console.enabled = true
      log.console.threshold = if (verbose) Level.TRACE
                              else Level.WARN
    }
  }
}

trait Module {
  type RawSchema
  type Schema
  type Context

  private val logger = Log.forName("module")
  def verbose: Boolean = false
  
  val encoding = "UTF-8"
  val newline = System.getProperty("line.separator")
  
  trait Importable {
    def targetNamespace: Option[String]
    def importNamespaces: Seq[String]
    def importLocations: Seq[String]
    def includeLocations: Seq[String]
    def raw: RawSchema
    def location: URI
    def toSchema(context: Context, outerNamespace: Option[String]): Schema
  }

  implicit val fileReader = new CanBeRawSchema[File, RawSchema] {
    override def toRawSchema(value: File) = readerToRawSchema(UnicodeFileReader.reader(value))
    override def toURI(value: File) = value.toURI
  }

  def process(file: File, packageName: String, outdir: File): List[File] =
    process(file, Config(packageNames = Map(None -> Some(packageName)), outdir = outdir) )
  
  def process(file: File, config: Config): List[File] =
    processFiles(List(file), config)
  
  def processFiles(files: Seq[File], config: Config): List[File] = {
    implicit val fileWriter = new CanBeWriter[File] {
      override def toWriter(value: File) = new PrintWriter(new java.io.OutputStreamWriter(
          new java.io.FileOutputStream(value), encoding))
      override def newInstance(packageName: Option[String], fileName: String) = {
        val dir = if (config.packageDir) packageDir(packageName, config.outdir)
                  else config.outdir
        dir.mkdirs()
        new File(dir, fileName)
      }

    }

    files.foreach(file => if (!file.exists)
      error("file not found: " + file.toString))
      
    val outfiles = processReaders(files, config)
    outfiles map { x => 
      println("generated " + x + ".")
      logger.info("generated " + x + ".") }
    outfiles
  }

  def packageDir(packageName: Option[String], dir: File) = packageName map { x =>
    (dir /: x.split('.')) { new File(_, _) }
  } getOrElse {dir}

  def processString(input: String, packageName: String): List[String] =
    processString(input, Config(packageNames = Map(None -> Some(packageName))))

  def processString(input: String, config: Config): List[String] = {
    implicit val stringReader = new CanBeRawSchema[String, RawSchema] {
      override def toRawSchema(value: String) = readerToRawSchema(new java.io.StringReader(value))
      override def toURI(value: String) = new URI("file://C:/temp.txt")
    }

    implicit val stringWriter = new CanBeWriter[java.io.StringWriter] {
      override def toWriter(value: java.io.StringWriter) = new PrintWriter(value)
      override def newInstance(packageName: Option[String], fileName: String) = new java.io.StringWriter
    }

    processReaders(Seq(input), config) map {_.toString}
  }

  def processNode(input: Node, packageName: String): List[String] =
    processNode(input, Config(packageNames = Map(None -> Some(packageName))))

  def processNode(input: Node, config: Config): List[String] = {
    implicit val nodeReader = new CanBeRawSchema[Node, RawSchema] {
      override def toRawSchema(value: Node) = nodeToRawSchema(value)
      override def toURI(value: Node) = new URI("file://C:/temp.txt")
    }

    implicit val stringWriter = new CanBeWriter[java.io.StringWriter] {
      override def toWriter(value: java.io.StringWriter) = new PrintWriter(value)
      override def newInstance(packageName: Option[String], fileName: String) = new java.io.StringWriter
    }

    processReaders(Seq(input), config) map {_.toString}
  }

  def headerSnippet(pkg: Option[String]): Snippet =
    Snippet(<source>// Generated by &lt;a href="http://scalaxb.org/"&gt;scalaxb&lt;/a&gt;.
{ pkg map { "package " + _ } getOrElse {""} }</source>)

  def processReaders[From, To](files: Seq[From], config: Config)
     (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): List[To] = {
    val snippets = ListBuffer.empty[Snippet]
    val context = buildContext

    logger.debug("%s" format files.toString())

    val importables0 = ListMap[From, Importable](files map { f =>
      f -> toImportable(ev.toURI(f), ev.toRawSchema(f))}: _*)
    val importables = Seq[(Importable, From)](files map { f => importables0(f) -> f }: _*)
    val schemas = ListMap[Importable, Schema](importables map { case (importable, file) =>
      val s = parse(importable, context, None)
      (importable, s) }: _*)

    val additionalImportables = ListMap.empty[Importable, File]

    // recursively add missing files
    def addMissingFiles() {
      val current = (importables map {_._1}) ++ additionalImportables.keysIterator.toList
      // check for all dependencies before proceeding.
      val missings = (current flatMap { importable =>
        missingDependencies(importable, current) }).distinct

      val additional = missings flatMap { x =>
        val uri = new URI(x)
        val file = new File(new File(uri.getPath).getName)

        if (file.exists) Some(file)
        else None
      }
      var added = false
      additionalImportables ++= (additional map { x =>
        logger.warn("added " + x + " to compilation.")
        added = true
        val importable = toImportable(implicitly[CanBeRawSchema[File, RawSchema]].toURI(x),
          implicitly[CanBeRawSchema[File, RawSchema]].toRawSchema(x))
        val s = parse(importable, context, None)
        schemas(importable) = s
        (importable, x) })
      if (added) addMissingFiles()
    }

    def toFileNamePart[From](file: From)(implicit ev: CanBeRawSchema[From, RawSchema]): String =
      """([.]\w+)$""".r.replaceFirstIn(new File(ev.toURI(file).getPath).getName, "")

    def processImportables[A](xs: List[(Importable, A)])(implicit ev: CanBeRawSchema[A, RawSchema]) = xs flatMap {
      case (importable, file) =>
        generate(schemas(importable), toFileNamePart(file), context, config) map { case (pkg, snippet, part) =>
          snippets += snippet
          val output = evTo.newInstance(pkg, part + ".scala")
          val out = evTo.toWriter(output)
          try {
            printNodes(snippet.definition, out)
          } finally {
            out.flush()
            out.close()
          }
          output
        }
    }

    def processProtocol = {
      val pkg = config.protocolPackageName match {
        case Some(_) => config.protocolPackageName
        case _ => packageName(importables0(files.head).targetNamespace, context)
      }
      val output = implicitly[CanBeWriter[To]].newInstance(pkg, config.protocolFileName)
      val out = implicitly[CanBeWriter[To]].toWriter(output)
      val config2 = config.copy(
        protocolPackageName = pkg,
        defaultNamespace = config.defaultNamespace match {
          case Some(_) => config.defaultNamespace
          case _ => importables0(files.head).targetNamespace
        }    
      )
      val protocolNodes = generateProtocol(Snippet(snippets: _*), context, config2)
      try {
        printNodes(protocolNodes, out)
      } finally {
        out.flush()
        out.close()
      }
      output
    }

    def processUnnamedIncludes() {
      logger.debug("processUnnamedIncludes")
      import scala.collection.mutable.{ListBuffer, ListMap}
      val all = (importables.toList map {_._1}) ++ (additionalImportables.toList map {_._1})
      val parents: ListBuffer[Importable] = ListBuffer(all filter { !_.includeLocations.isEmpty}: _*)
      def children(importable: Importable): List[Importable] = {
        val uris = importable.includeLocations map { includedLoc => shortenUri(new URI(includedLoc)) }
        all filter { x => uris contains shortenUri(x.location) }
      }
      val mapping: ListMap[Importable, Option[String]] = ListMap()

      val len = parents.size * parents.size
      for {
        i <- 0 to len if parents.size > 0
      } {
        val parent = parents(i % parents.size)
        val xs = children(parent)
        if (xs forall { x => !(parents contains x) }) {
          val tns = mapping.get(parent) getOrElse {parent.targetNamespace}
          tns foreach { tnsstr => xs foreach { x =>
            x.targetNamespace match {
              case Some(ns) =>
              case None =>
                logger.debug("processUnnamedIncludes - setting %s's outer namespace to %s" format (x.location, tnsstr))
                schemas(x) = parse(x, context, tns)
            }
            mapping(x) = tns
          }}
          parents.remove(i % parents.size)
        }
      }
    }

    addMissingFiles()
    processUnnamedIncludes()
    processContext(context, schemas.valuesIterator.toSeq, config)
    processImportables(importables.toList) :::
    processImportables(additionalImportables.toList) :::
    List(processProtocol) :::
    (if (config.generateRuntime) generateRuntimeFiles[To](context)
     else Nil)
  }

  def generateFromResource[To](packageName: Option[String], fileName: String, resourcePath: String)
                              (implicit evTo: CanBeWriter[To]) = {
    val output = implicitly[CanBeWriter[To]].newInstance(packageName, fileName)
    val out = implicitly[CanBeWriter[To]].toWriter(output)
    try {
      printFromResource(resourcePath, out)
    } finally {
      out.flush()
      out.close()
    }
    output
  }

  def generateRuntimeFiles[To](context: Context)(implicit evTo: CanBeWriter[To]): List[To]

  // returns a seq of package name, snippet, and file name part tuple
  def generate(schema: Schema, part: String, context: Context, config: Config): Seq[(Option[String], Snippet, String)]
    
  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node]
    
  def toImportable(location: URI, rawschema: RawSchema): Importable

  def shortenUri(uri: URI): String = {
    val path = Option[String](uri.getPath) getOrElse {""}
    (new File(path)).getName
  }

  def missingDependencies(importable: Importable, files: Seq[Importable]): List[String] = {
    val nsBased = importable.importNamespaces.toList flatMap { ns =>
      files filter { _.targetNamespace == ns }
    }
    val XML_LOCATION = "http://www.w3.org/2001/xml.xsd"
    val locationBased = importable.importLocations.toList flatMap { loc =>
      val deps = files filter { f => shortenUri(f.location) == shortenUri(new URI(loc)) }
      if (deps.isEmpty && loc != XML_LOCATION) {
        logger.warn((new File(importable.location.getPath).getName) + " imports " + loc +
        " but no schema with that name was compiled together.")
        List(loc)
      }
      else Nil
    }
    val includes = importable.includeLocations.toList flatMap { loc =>
      val deps = files filter { f => shortenUri(f.location) == shortenUri(new URI(loc)) }
      if (deps.isEmpty && loc != XML_LOCATION) {
        logger.warn("Warning: " + (new File(importable.location.getPath).getName) + " includes " + loc +
          " but no schema with that name was compiled together.")
        List(loc)
      }
      else Nil
    }

    (locationBased ::: includes).distinct
    // (nsBased ::: locationBased ::: includes).distinct
  }

  def buildContext: Context
  
  def processContext(context: Context, schemas: Seq[Schema], config: Config): Unit

  def packageName(namespace: Option[String], context: Context): Option[String]

  def readerToRawSchema(reader: Reader): RawSchema

  def nodeToRawSchema(node: Node): RawSchema

  def parse(importable: Importable, context: Context, outerNamespace: Option[String]): Schema
    = importable.toSchema(context, outerNamespace)
    
  def parse(location: URI, in: Reader, outerNamespace: Option[String]): Schema
    = parse(toImportable(location, readerToRawSchema(in)), buildContext, outerNamespace)
    
  def printNodes(nodes: Seq[Node], out: PrintWriter) {
    import scala.xml._
        
    def printNode(n: Node): Unit = n match {
      case Text(s)          => out.print(s)
      case EntityRef("lt")  => out.print('<')
      case EntityRef("gt")  => out.print('>')
      case EntityRef("amp") => out.print('&')
      case atom: Atom[_]    => out.print(atom.text)
      case elem: Elem       =>
        printNodes(elem.child, out)
        if (elem.text != "") {
          if (elem.text.contains(newline)) out.println("")
          out.println("")
        }
      case _                => logger.error("error in Module: encountered "
        + n.getClass() + " " + n.toString)
    }
    
    for (node <- nodes) { printNode(node) }
  }

  def printFromResource(source: String, out: PrintWriter) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    var line: Option[String] = None
    line = Option[String](reader.readLine)
    while (line != None) {
      line foreach { out.println }
      line = Option[String](reader.readLine)
    }
    in.close
    out.flush
  }

  def copyFileFromResource(source: String, dest: File) =
    printFromResource(source, new java.io.PrintWriter(new java.io.FileWriter(dest)))

  def mergeSnippets(snippets: Seq[Snippet]) =
    Snippet(snippets flatMap {_.definition},
      snippets flatMap {_.companion},
      snippets flatMap {_.defaultFormats},
      snippets flatMap {_.implicitValue})
}

object CustomXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val factory = javax.xml.parsers.SAXParserFactory.newInstance()
    factory.setFeature("http://xml.org/sax/features/validation", false)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    factory.newSAXParser()
  }
}

object UnicodeFileReader {
  def reader(value: File): Reader = {
    val BOM_SIZE = 4
    val EF = 0xEF.toByte
    val BB = 0xBB.toByte
    val BF = 0xBF.toByte
    val FE = 0xFE.toByte
    val FF = 0xFF.toByte
    val bom = Array.ofDim[Byte](BOM_SIZE)
    val in = new java.io.PushbackInputStream(new java.io.FileInputStream(value), BOM_SIZE)
    val readSize = in.read(bom, 0, bom.length)
    val (bomSize, encoding) = bom.toList match {
      case EF :: BB :: BF :: xs => (3, "UTF-8")
      case FE :: FF :: xs       => (2, "UTF-16BE")
      case FF :: FE :: xs       => (2, "UTF-16LE")
      case _                    => (0, "UTF-8")
    }
    in.unread(bom, bomSize, readSize - bomSize)
    new BufferedReader(new java.io.InputStreamReader(in, encoding))
  }
}

class ReferenceNotFound(kind: String, namespace: Option[String], name: String) extends RuntimeException(
  "Error: Referenced " + kind + " " +
    (namespace map { "{" + _ + "}" } getOrElse {"(unqualified) "}) + name + " was not found.")

class CaseClassTooLong(fqn: String, xmlname: String) extends RuntimeException(
  """Error: A case class with > 22 parameters cannot be created for %s. Consider using --wrap-contents "%s" option.""".format(
    fqn, xmlname
  )
)
