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
import java.io.{File, BufferedReader, Reader, PrintWriter}
import java.net.{URI}
import scala.xml.{Node, Elem}
import scala.xml.factory.{XMLLoader}
import javax.xml.parsers.SAXParser

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  paramPrefix: Option[String] = None,
  outdir: File = new File("."),
  wrappedComplexTypes: List[String] = Nil,
  primaryNamespace: Option[String] = None,
  seperateProtocol: Boolean = true)

case class Snippet(definition: Seq[Node],
  companion: Seq[Node] = <source/>,
  implicitValue: Seq[Node]  = <source/>)

trait CanBeWriter[A] {
 def toWriter(value: A): PrintWriter
 def newInstance(fileName: String): A
}

trait CanBeRawSchema[A, B] {
  def toRawSchema(value: A): B
  def toURI(value: A): URI
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
    def out: PrintWriter
    def location: URI
    def toSchema(context: Context): Schema
  }

  implicit val fileReader = new CanBeRawSchema[File, RawSchema] {
    override def toRawSchema(value: File) = {
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
      readerToRawSchema(new BufferedReader(new java.io.InputStreamReader(in, encoding)))
    }
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
      override def newInstance(fileName: String) = new File(config.outdir, fileName)
    }

    files.foreach(file => if (!file.exists)
      error("file not found: " + file.toString))
      
    val outfiles = processReaders(files, config)
    outfiles map { x => println("generated " + x + ".") }
    outfiles
  }

  def processString(input: String, packageName: String): List[String] =
    processString(input, Config(packageNames = Map(None -> Some(packageName))))

  def processString(input: String, config: Config): List[String] = {
    implicit val stringReader = new CanBeRawSchema[String, RawSchema] {
      override def toRawSchema(value: String) = readerToRawSchema(new java.io.StringReader(value))
      override def toURI(value: String) = new URI("file://C:/temp.txt")
    }

    implicit val stringWriter = new CanBeWriter[java.io.StringWriter] {
      override def toWriter(value: java.io.StringWriter) = new PrintWriter(value)
      override def newInstance(fileName: String) = new java.io.StringWriter
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
      override def newInstance(fileName: String) = new java.io.StringWriter
    }

    processReaders(Seq(input), config) map {_.toString}
  }

  def toOutput[From, To](file: From)
      (implicit ev: CanBeRawSchema[From, RawSchema], evTo: CanBeWriter[To]): To =
    evTo.newInstance("""([.]\w+)$""".r.replaceFirstIn(
      new File(ev.toURI(file).getPath).getName, ".scala"))

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
    val outputs = ListBuffer.empty[To]
    val importables = ListBuffer.empty[Importable]
    importables ++= (files.toList map { x =>
      val out = toOutput(x)
      outputs += out
      toImportable(ev.toURI(x), ev.toRawSchema(x), evTo.toWriter(out))
    })

    val config: Config = config0.primaryNamespace map { _ => config0 } getOrElse {
      val pns: Option[String] = importables.head.targetNamespace
      config0.copy(primaryNamespace = pns)
    }    
    
    val schemas = ListMap[Importable, Schema](importables map { file =>
      (file, parse(file, context)) }: _*)

    // check for all dependencies before proceeding.
    val missings = (importables flatMap { importable =>
      missingDependencies(importable, importables.toList) }).distinct

    val additional = missings flatMap  { x =>
      val uri = new URI(x)
      val file = new File(new File(uri.getPath).getName)

      if (file.exists) Some(file)
      else None
    }
    importables ++= (additional map { x =>
      println("Warning: added " + x + " to compilation.")

      val out = toOutput(x)
      outputs += out
      val importable = toImportable(implicitly[CanBeRawSchema[File, RawSchema]].toURI(x),
        implicitly[CanBeRawSchema[File, RawSchema]].toRawSchema(x),
        evTo.toWriter(out))
      schemas(importable) = parse(importable, context)
      importable
    })

    processContext(context, config)
    
    importables foreach { importable =>
      val schema = schemas(importable)
      val out = importable.out
      try {    
        val snippet = generate(schema, context, config)
        splitSnippet(snippet)
        printNodes(snippet.definition, out)
      } finally {
        out.flush()
        out.close()
      }
    }

    val outProtocol = implicitly[CanBeWriter[To]].newInstance("xmlprotocol.scala")
    val outProtocolWriter = implicitly[CanBeWriter[To]].toWriter(outProtocol)
    val protocolNodes = generateProtocol(Snippet(nodes, companions, implicitValues), context, config)
    try {
      printNodes(protocolNodes, outProtocolWriter)
    } finally {
      outProtocolWriter.flush()
      outProtocolWriter.close()
    }

    val outRuntime = implicitly[CanBeWriter[To]].newInstance("scalaxb.scala")
    val outRuntimeWriter = implicitly[CanBeWriter[To]].toWriter(outRuntime)
    try {
      printFromResource("/scalaxb.scala.template", outRuntimeWriter)
    } finally {
      outRuntimeWriter.flush()
      outRuntimeWriter.close()
    }

    outputs.toList ::: List(outProtocol, outRuntime)
  }
  
  def generate(schema: Schema, context: Context, config: Config): Snippet
    
  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node]
    
  def toImportable(location: URI, rawschema: RawSchema, out: PrintWriter): Importable
  
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

  def readerToRawSchema(reader: Reader): RawSchema

  def nodeToRawSchema(node: Node): RawSchema

  def parse(importable: Importable, context: Context): Schema
    = importable.toSchema(context)
    
  def parse(location: URI, in: Reader, out: PrintWriter): Schema
    = parse(toImportable(location, readerToRawSchema(in), out), buildContext)
    
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
