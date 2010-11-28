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
import scala.xml.{Node}

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  paramPrefix: Option[String] = None,
  wrappedComplexTypes: List[String] = Nil,
  primaryNamespace: Option[String] = None,
  seperateProtocol: Boolean = true)

case class Snippet(definition: Seq[Node],
  companion: Seq[Node] = <source/>,
  implicitValue: Seq[Node]  = <source/>)
    
trait Module extends Logger {
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
    def reader: Reader
    def out: PrintWriter
    def location: URI
    def toSchema(context: Context): Schema
  }
  
  def process(file: File, output: File, outProtocol: File, packageName: String): List[File] =
    process(file, output, outProtocol,
      Config(packageNames = Map(None -> Some(packageName))))
  
  def process(file: File, output: File, outProtocol: File, config: Config): List[File] = 
    processFiles(List((file, output)), outProtocol, config)
  
  def processFiles(filePairs: Seq[(File, File)],
      outProtocol: File, config: Config): List[File] = {    
    val files = filePairs.map(_._1)
    files.foreach(file => if (!file.exists)
      error("file not found: " + file.toString))
      
    processReaders( (filePairs map { pair =>
        (pair._1.toURI,
        new BufferedReader(new java.io.InputStreamReader(
          new java.io.FileInputStream(pair._1), encoding)),
        new PrintWriter(new java.io.OutputStreamWriter(
          new java.io.FileOutputStream(pair._2), encoding)) )
      }),
      new PrintWriter(new java.io.OutputStreamWriter(
        new java.io.FileOutputStream(outProtocol), encoding)),
      config)
    val outfiles = ListBuffer.empty[File] ++ (filePairs map { _._2 })
    outfiles foreach { file =>
      println("generated " + file + ".")
    }
    outfiles += outProtocol
    println("generated " + outProtocol + ".")
    
    if (filePairs.size > 0) {
      val parent = filePairs(0)._2.getParentFile
      val helper = new File(parent, "Helper.scala")
      copyFileFromResource("/Helper.scala", helper)
      outfiles += helper
      println("generated " + helper + ".")
    }
    
    outfiles.toList
  }
  
  def processReaders(fileTriplets: Seq[(URI, Reader, PrintWriter)],
      outProtocol: PrintWriter, config0: Config) = {    
    val companions = ListBuffer.empty[Node]
    val implicitValues = ListBuffer.empty[Node]
    val nodes = ListBuffer.empty[Node]
    def splitSnippet(snippet: Snippet) {
      nodes ++= snippet.definition
      companions ++= snippet.companion
      implicitValues ++= snippet.implicitValue
    }
    
    val context = buildContext
    val importables = fileTriplets.toList map { x => toImportable(x._1, x._2, x._3) }
    val config: Config = config0.primaryNamespace map { _ => config0 } getOrElse {
      val pns: Option[String] = importables.head.targetNamespace
      config0.copy(primaryNamespace = pns)
    }    
    
    val schemas = Map[Importable, Schema](importables map { file =>
      (file, parse(file, context)) }: _*)
    processContext(context, config)
    
    // check for all dependencies before proceeding.
    importables foreach { importable => 
      val dependents = dependentFiles(importable, importables) map { i => schemas(i) }
    }
    
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
    
    val protocolNodes = generateProtocol(Snippet(nodes, companions, implicitValues), context, config)
    try {
      printNodes(protocolNodes, outProtocol)
    } finally {
      outProtocol.flush()
      outProtocol.close()      
    }
  }
  
  def generate(schema: Schema, context: Context, config: Config): Snippet
    
  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node]
    
  def toImportable(location: URI, in: Reader, out: PrintWriter): Importable
  
  def dependentFiles(importable: Importable, files: List[Importable]): List[Importable] = {
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
      if (deps.isEmpty && loc != XML_LOCATION) println("Warning: " + importable.location.toString + " imports " + loc +
        " but no schema with that name was compiled together.")
      deps
    }
    val includes = importable.includeLocations.toList flatMap { loc =>
      val deps = files filter { f => shorten(f.location) == shorten(new URI(loc)) }
      if (deps.isEmpty) println("Warning: " + importable.location.toString + " includes " + loc +
        " but no schema with that name was compiled together.")
      deps
    }
    
    (nsBased ::: locationBased ::: includes).distinct
  }
    
  def buildContext: Context
  
  def processContext(context: Context, config: Config): Unit
  
  def parse(importable: Importable, context: Context): Schema
    = importable.toSchema(context)
    
  def parse(location: URI, in: Reader, out: PrintWriter): Schema
    = parse(toImportable(location, in, out), buildContext)
  
  def parse(inFile: File, outFile: File): Schema
    = parse(inFile.toURI,
        new BufferedReader(new java.io.InputStreamReader(
          new java.io.FileInputStream(inFile), encoding)),
        new PrintWriter(new java.io.OutputStreamWriter(
          new java.io.FileOutputStream(outFile), encoding)))
    
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
  
  def copyFileFromResource(source: String, dest: File) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.PrintWriter(new java.io.FileWriter(dest))
    var line: Option[String] = None
    line = Option[String](reader.readLine)
    while (line != None) {
      line foreach { out.println }
      line = Option[String](reader.readLine)
    }
    in.close
    out.flush
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
