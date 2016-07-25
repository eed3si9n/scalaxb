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

import scalashim._
import java.net.{URI}
import scala.xml.{Node, Elem, UnprefixedAttribute, NamespaceBinding}
import scala.xml.factory.{XMLLoader}
import javax.xml.parsers.SAXParser
import java.io.{File, PrintWriter, Reader, BufferedReader}
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ListMap}
import ConfigEntry._

object Snippet {
  def apply(snippets: Snippet*): Snippet =
    Snippet(snippets flatMap { s => s.companion ++ s.definition},
      Nil,
      snippets flatMap {_.defaultFormats},
      snippets flatMap {_.implicitValue},
      snippets flatMap {_.elemToTypeClauses})
}

case class Snippet(definition: Seq[Node] = Nil,
  companion: Seq[Node] = Nil,
  defaultFormats: Seq[Node] = Nil,
  implicitValue: Seq[Node] = Nil,
  elemToTypeClauses: Seq[Node] = Nil)

trait CanBeWriter[A] {
 def toWriter(value: A): PrintWriter
 def newInstance(packageName: Option[String], fileName: String): A
}
object CanBeWriter {
  implicit val stringWriter = new CanBeWriter[java.io.StringWriter] {
    override def toWriter(value: java.io.StringWriter) = new PrintWriter(value)
    override def newInstance(packageName: Option[String], fileName: String) = new java.io.StringWriter
  }
}

trait CanBeRawSchema[A, B] {
  def toRawSchema(value: A): B
  def toURI(value: A): URI
}

object Module {
  import scala.util.matching.Regex

  val NL = System.getProperty("line.separator")
  val FileExtension = """.*([.]\w+)$""".r

  def moduleByFileName(file: File): Module = {
    file.toString match {
      case FileExtension(".wsdl") =>
        new scalaxb.compiler.wsdl11.Driver
      case _ =>
        new scalaxb.compiler.xsd.Driver
    }
  }

  def splitTypeName(value: String, scope: scala.xml.NamespaceBinding) = masked.scalaxb.Helper.splitQName(value, scope)

  def indent(n: Int) = "  " * n

  def camelCase(name: String): String = {
    val (cap, rest) = name span {_.isUpper}
    cap.size match {
      case x if (x == 0) || (x == 1) || (x == name.size) => cap.toLowerCase + rest
      case x => (cap take (x - 1)).toLowerCase + (cap drop (x - 1)) + rest
    }
  }

}

trait Module {
  type RawSchema
  type Schema
  type Context

  case class CompileSource[From](context: Context,
    schemas: ListMap[Importable, Schema],
    importables: Seq[(Importable, From)],
    additionalImportables: ListMap[Importable, File],
    firstNamespace: Option[String]) {

  }

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
    def toSchema(context: Context): Schema
    def swapTargetNamespace(outerNamespace: Option[String], n: Int): Importable
  }

  implicit val fileReader = new CanBeRawSchema[File, RawSchema] {
    override def toRawSchema(value: File) = readerToRawSchema(UnicodeFileReader.reader(value))
    override def toURI(value: File) = value.toURI
  }
  val stringReader = new CanBeRawSchema[String, RawSchema] {
    override def toRawSchema(value: String) = readerToRawSchema(new java.io.StringReader(value))
    override def toURI(value: String) = new URI("file://C:/temp.txt")
  }
  val nodeReader = new CanBeRawSchema[Node, RawSchema] {
    override def toRawSchema(value: Node) = nodeToRawSchema(value)
    override def toURI(value: Node) = new URI("file://C:/temp.txt")
  }

  def process(file: File, packageName: String, outdir: File): List[File] =
    process(file, Config.default.
      update(PackageNames(Map(None -> Some(packageName)))).
      update(Outdir(outdir)))

  def process(file: File, config: Config): List[File] =
    processFiles(List(file), config)

  def processFiles(files: Seq[File], config: Config): List[File] = {
    val (source, outfiles) = infoFiles(files, config)
    outfiles map { x =>
      println("generated " + x + ".")
      logger.info("generated " + x + ".") }
    outfiles
  }

  def infoFiles(files: Seq[File], config: Config): (CompileSource[File], List[File]) = {
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
      sys.error("file not found: " + file.toString))
    processReaders(files, config)
  }

  def packageDir(packageName: Option[String], dir: File) = packageName map { x =>
    (dir /: x.split('.')) { new File(_, _) }
  } getOrElse {dir}

  def processString(input: String, packageName: String): List[String] =
    processString(input, Config.default.update(PackageNames(Map(None -> Some(packageName)))))

  def processString(input: String, config: Config): List[String] =
    infoString(input, config)._2

  def infoString(input: String, config: Config): (CompileSource[String], List[String]) = {
    implicit val ev = stringReader
    val (source, result) = processReaders(Seq(input), config)
    (source, result map {_.toString})
  }

  def processNode(input: Node, packageName: String): List[String] =
    processNode(input, Config.default.update(PackageNames(Map(None -> Some(packageName)))))

  def processNode(input: Node, config: Config): List[String] =
    infoNode(input, config)._2

  def infoNode(input: Node, config: Config): (CompileSource[Node], List[String]) = {
    implicit val ev = nodeReader
    val (source, result) = processReaders(Seq(input), config)
    (source, result map {_.toString})
  }

  def headerSnippet(pkg: Option[String]): Snippet =
    Snippet(<source>// Generated by &lt;a href="http://scalaxb.org/"&gt;scalaxb&lt;/a&gt;.
{ pkg map { "package " + _ } getOrElse {""} }</source>)

  def processReaders[From, To](files: Seq[From], config: Config)
     (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): (CompileSource[From], List[To]) = {
    val source = buildCompileSource(files)
    (source, processCompileSource(source, config))
  }

  def buildCompileSource[From, To](files: Seq[From])
     (implicit ev: CanBeRawSchema[From, RawSchema]): CompileSource[From] = {

    logger.debug("%s", files.toString())
    val context = buildContext
    val importables0 = ListMap[From, Importable](files map { f =>
      f -> toImportable(ev.toURI(f), ev.toRawSchema(f))}: _*)
    val importables = ListBuffer[(Importable, From)](files map { f => importables0(f) -> f }: _*)
    val schemas = ListMap[Importable, Schema](importables map { case (importable, file) =>
      val s = parse(importable, context)
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
        val s = parse(importable, context)
        schemas(importable) = s
        (importable, x) })
      if (added) addMissingFiles()
    }
    def processUnnamedIncludes() {
      logger.debug("processUnnamedIncludes")
      val all = (importables.toList map {_._1}) ++ (additionalImportables.toList map {_._1})
      val parents: ListBuffer[Importable] = ListBuffer(all filter { !_.includeLocations.isEmpty}: _*)
      def children(importable: Importable): List[Importable] = {
        val uris = importable.includeLocations map { includedLoc => shortenUri(new URI(includedLoc)) }
        all filter { x => uris contains shortenUri(x.location) }
      }
      val mapping: ListMap[Importable, Option[String]] = ListMap()
      val used: ListBuffer[Importable] = ListBuffer()
      var count: Int = 0
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
                logger.debug("processUnnamedIncludes - setting %s's outer namespace to %s", x.location, tnsstr)
                count += 1
                val swap = x.swapTargetNamespace(tns, count)
                schemas(swap) = parse(swap, context)
                additionalImportables(swap) = new File(swap.location.getPath)
                used += x
            }
            mapping(x) = tns
          }}
          parents.remove(i % parents.size)
        }
      }
      used.toList.distinct foreach { x =>
        schemas -= x
        logger.debug("processUnnamedIncludes - removing %s", x.location)
        val idx = importables.indexWhere { case (i, _) => i == x }
        if (idx >= 0) {
          importables remove idx
        }
        if (additionalImportables contains x) {
          additionalImportables -= x
        }
      }
    }

    addMissingFiles()
    processUnnamedIncludes()
    CompileSource(context, schemas, importables, additionalImportables,
      importables0(files.head).targetNamespace)
  }

  // ev: CanBeRawSchema[From, RawSchema]
  def processCompileSource[From, To](cs: CompileSource[From], config: Config)
     (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): List[To] = {
    val snippets = ListBuffer.empty[Snippet]

    def toFileNamePart[From](file: From)(implicit ev: CanBeRawSchema[From, RawSchema]): String =
      """([.]\w+)$""".r.replaceFirstIn(new File(ev.toURI(file).getPath).getName, "")

    def processImportables[A](xs: List[(Importable, A)])(implicit ev: CanBeRawSchema[A, RawSchema]) = xs flatMap {
      case (importable, file) =>
        generate(cs.schemas(importable), toFileNamePart(file), cs.context, config) map { case (pkg, snippet, part) =>
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
        case _ => packageName(cs.firstNamespace, cs.context)
      }
      val output = implicitly[CanBeWriter[To]].newInstance(pkg, config.protocolFileName)
      val out = implicitly[CanBeWriter[To]].toWriter(output)
      val config2 = config.update(ProtocolPackageName(pkg)).
        update(DefaultNamespace(config.defaultNamespace match {
          case Some(_) => config.defaultNamespace
          case _ => cs.firstNamespace
        }))
      val protocolNodes = generateProtocol(Snippet(snippets: _*), cs.context, config2)
      try {
        printNodes(protocolNodes, out)
      } finally {
        out.flush()
        out.close()
      }
      output
    }

    processContext(cs.context, cs.schemas.valuesIterator.toSeq, config)
    cs.schemas.valuesIterator.toSeq foreach { schema =>
      processSchema(schema, cs.context, config)
    }
    processImportables(cs.importables.toList) :::
    processImportables(cs.additionalImportables.toList) :::
    List(processProtocol) :::
    (if (config.generateRuntime) generateRuntimeFiles[To](cs.context, config)
     else Nil)
  }

  def generateFromResource[To](packageName: Option[String], fileName: String, resourcePath: String, placeholders: Map[String, String] = Map())
                              (implicit evTo: CanBeWriter[To]) = {
    val output = implicitly[CanBeWriter[To]].newInstance(packageName, fileName)
    val out = implicitly[CanBeWriter[To]].toWriter(output)
    
    // Placeholder replacement mechanics: find placeholders of the #{name}
    // form, resolve the names in the map and replace them.
    val map: String => String =
      """#\{([\w\d_]+)\}""".r.replaceAllIn(_, m => placeholders.get(m.group(1)).getOrElse(m.matched))

    try {
      printFromResource(resourcePath, out, map)
    } finally {
      out.flush()
      out.close()
    }
    output
  }

  def generateRuntimeFiles[To](context: Context, config: Config)(implicit evTo: CanBeWriter[To]): List[To]

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

  def processSchema(schema: Schema, context: Context, config: Config): Unit

  def processContext(context: Context, schemas: Seq[Schema], config: Config): Unit

  def packageName(namespace: Option[String], context: Context): Option[String]

  def readerToRawSchema(reader: Reader): RawSchema

  def nodeToRawSchema(node: Node): RawSchema

  def parse(importable: Importable, context: Context): Schema
    = importable.toSchema(context)

  def parse(location: URI, in: Reader): Schema
    = parse(toImportable(location, readerToRawSchema(in)), buildContext)

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

  def printFromResource(source: String, out: PrintWriter, map: String => String = identity) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    var line: Option[String] = None
    line = Option[String](reader.readLine)
    while (line != None) {
      line.map(map) foreach { out.println }
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

  def appendPostFix(location: URI, n: Int): URI = new URI(shortenUri(location).replaceFirst("\\.xsd", "") + n.toString + ".xsd")
  // replace the targetNamespace
  def replaceNamespace(raw: Node, old: Option[String], outerNamespace: Option[String]): Node = {
    def fixScope(scope: NamespaceBinding): NamespaceBinding =
      NamespaceBinding(null, outerNamespace getOrElse null, scope)
    def fixSeq(ns: Seq[Node]): Seq[Node] =
      for { node <- ns } yield node match {
        case elem: Elem => 
          elem.copy(scope = fixScope(elem.scope),
               child = fixSeq(elem.child))
        case other => other
      }
    val xml = CustomXML.load(new java.io.StringReader((raw match {
      case elem: Elem if !old.isDefined =>
        val x = elem.copy(attributes = new UnprefixedAttribute("targetNamespace", outerNamespace getOrElse null, elem.attributes),
          scope = fixScope(elem.scope), child = fixSeq(elem.child))
        x
      case node => node
    }).toString))
    xml
  }
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
