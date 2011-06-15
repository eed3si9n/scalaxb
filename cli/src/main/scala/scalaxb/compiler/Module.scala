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

import scala.collection.{Map, Set}
import scala.collection.mutable.{ListBuffer, ListMap}
import java.net.{URI}
import scala.xml.{Node, Elem}
import scala.xml.factory.{XMLLoader}
import javax.xml.parsers.SAXParser
import java.io.{File, PrintWriter, Reader, BufferedReader}

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  classPostfix: Option[String] = None,
  paramPrefix: Option[String] = None,
  outdir: File = new File("."),
  packageDir: Boolean = false,
  wrappedComplexTypes: List[String] = Nil,
  primaryNamespace: Option[String] = None,
  seperateProtocol: Boolean = true,
  generateRuntime: Boolean = true)

case class Snippet(definition: Seq[Node],
  companion: Seq[Node] = <source/>,
  implicitValue: Seq[Node]  = <source/>)

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

  def moduleByFileName(file: File, verbose: Boolean): Module = file.toString match {
    case FileExtension(".wsdl") =>
      if (verbose) new scalaxb.compiler.wsdl11.Driver with Verbose
      else new scalaxb.compiler.wsdl11.Driver
    case _ =>
      if (verbose) new scalaxb.compiler.xsd.Driver with Verbose
      else new scalaxb.compiler.xsd.Driver
  }

  def splitTypeName(name: String, scope: scala.xml.NamespaceBinding): (Option[String], String) =
    if (name.contains(':')) {
      val prefix = name.dropRight(name.length - name.indexOf(':'))
      val value = name.drop(name.indexOf(':') + 1)
      Option[String](scope.getURI(prefix)) -> value
    } else (Option[String](scope.getURI(null)), name)

  def indent(n: Int) = "  " * n

  def camelCase(name: String): String = {
    val (cap, rest) = name span {_.isUpper}
    cap.size match {
      case x if (x == 0) || (x == 1) || (x == name.size) => cap.toLowerCase + rest
      case x => (cap take (x - 1)).toLowerCase + (cap drop (x - 1)) + rest
    }
  }
}

trait Module extends Logger {
  type RawSchema
  type Schema
  type Context

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
    outfiles map { x => println("generated " + x + ".") }
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

  def toOutput[From, To](packageName: Option[String], file: From)
      (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): To =
    evTo.newInstance(packageName, toFileNamePart(file) + ".scala")

  def toFileNamePart[From](file: From)(implicit ev: CanBeRawSchema[From, RawSchema]): String =
    """([.]\w+)$""".r.replaceFirstIn(new File(ev.toURI(file).getPath).getName, "")

  def processReaders[From, To](files: Seq[From], config0: Config)
     (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): List[To] = {
    val companions = ListBuffer.empty[Node]
    val implicitValues = ListBuffer.empty[Node]
    val nodes = ListBuffer.empty[Node]
    def splitSnippet(snippet: Snippet) {
      nodes ++= snippet.definition
      companions ++= snippet.companion
      implicitValues ++= snippet.implicitValue
    }

    val context = buildContext
    val importables = ListMap[Importable, From](files map { file =>
      (toImportable(ev.toURI(file), ev.toRawSchema(file)), file)}: _*)
    val config: Config = config0.primaryNamespace map { _ => config0 } getOrElse {
      val pns: Option[String] = importables.head._1.targetNamespace
      config0.copy(primaryNamespace = pns)
    }    
    
    val schemas = ListMap[Importable, Schema](importables.toList map { case (importable, file) =>
      (importable, parse(importable, context)) }: _*)

    val additionalImportables = ListMap.empty[Importable, File]

    // recursively add missing files
    def addMissingFiles() {
      val current = importables.keysIterator.toList ::: additionalImportables.keysIterator.toList
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
        println("Warning: added " + x + " to compilation.")
        added = true
        val importable = toImportable(implicitly[CanBeRawSchema[File, RawSchema]].toURI(x),
          implicitly[CanBeRawSchema[File, RawSchema]].toRawSchema(x))
        schemas(importable) = parse(importable, context)
        (importable, x) })
      if (added) addMissingFiles()
    }

    addMissingFiles()
    processContext(context, config)

    def processImportables[A](xs: List[(Importable, A)])(implicit ev: CanBeRawSchema[A, RawSchema]) = xs map {
      case (importable, file) =>
        val schema = schemas(importable)
        val output = toOutput(packageName(importable.targetNamespace, context), file)
        val out = evTo.toWriter(output)
        try {
          val snippet = generate(schema, context, config)
          splitSnippet(snippet)
          printNodes(snippet.definition, out)
        } finally {
          out.flush()
          out.close()
        }
        output
    }

    def processProtocol = {
      val output = implicitly[CanBeWriter[To]].newInstance(packageName(None, context),
        toFileNamePart(files.head) + "_xmlprotocol.scala")
      val out = implicitly[CanBeWriter[To]].toWriter(output)
      val protocolNodes = generateProtocol(Snippet(nodes, companions, implicitValues), context, config)
      try {
        printNodes(protocolNodes, out)
      } finally {
        out.flush()
        out.close()
      }
      output
    }

    processImportables(importables.toList) :::
    processImportables(additionalImportables.toList) :::
    List(processProtocol) :::
    (if (config.generateRuntime) generateRuntimeFiles[To]
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

  def generateRuntimeFiles[To](implicit evTo: CanBeWriter[To]): List[To]

  def generate(schema: Schema, context: Context, config: Config): Snippet
    
  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node]
    
  def toImportable(location: URI, rawschema: RawSchema): Importable
  
  def missingDependencies(importable: Importable, files: List[Importable]): List[String] = {
    def shorten(uri: URI): String = {
      val path = Option[String](uri.getPath) getOrElse {""}
      (new File(path)).getName
    }
    
    val nsBased = importable.importNamespaces.toList flatMap { ns =>
      files filter { _.targetNamespace == ns }
    }
    val XML_LOCATION = "http://www.w3.org/2001/xml.xsd"
    val locationBased = importable.importLocations.toList flatMap { loc =>
      val deps = files filter { f => shorten(f.location) == shorten(new URI(loc)) }
      if (deps.isEmpty && loc != XML_LOCATION) {
        println("Warning: " + (new File(importable.location.getPath).getName) + " imports " + loc +
        " but no schema with that name was compiled together.")
        List(loc)
      }
      else Nil
    }
    val includes = importable.includeLocations.toList flatMap { loc =>
      val deps = files filter { f => shorten(f.location) == shorten(new URI(loc)) }
      if (deps.isEmpty && loc != XML_LOCATION) {
        println("Warning: " + (new File(importable.location.getPath).getName) + " includes " + loc +
          " but no schema with that name was compiled together.")
        List(loc)
      }
      else Nil
    }

    (locationBased ::: includes).distinct
    // (nsBased ::: locationBased ::: includes).distinct
  }
    
  def buildContext: Context
  
  def processContext(context: Context, config: Config): Unit

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
      case _                => log("error in Module: encountered "
        + n.getClass() + " " + n.toString)
    }
    
    for (node <- nodes) { printNode(node) }
  }
  
  override def log(msg: String) {
    if (verbose) {
      println("["+msg+"]")
      Console.flush
    }
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

trait Verbose extends Module {
  override val verbose = true
}

trait Logger {
  def log(msg: String) {
    println("["+msg+"]")
    Console.flush
  }
}

class ReferenceNotFound(kind: String, namespace: Option[String], name: String) extends RuntimeException(
  "Error: Referenced " + kind + " " +
    (namespace map { "{" + _ + "}" } getOrElse {""}) + name + " was not found.")

class CaseClassTooLong(fqn: String, xmlname: String) extends RuntimeException(
  """Error: A case class with > 22 parameters cannot be created for %s. Consider using --wrap-contents "%s" option.""".format(
    fqn, xmlname
  )
)
