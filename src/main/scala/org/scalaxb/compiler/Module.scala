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
 
package org.scalaxb.compiler

import scala.collection.{Map, Set}
import scala.collection.mutable.{ListBuffer, ListMap}
import java.io.{File, BufferedReader, Reader, PrintWriter}
import scala.xml.{Node}

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  paramPrefix: Option[String] = None,
  wrappedComplexTypes: List[String] = Nil)

trait Module extends Logger {
  type Schema
  type Context
  
  def verbose: Boolean = false
  
  val encoding = "UTF-8"
  val newline = System.getProperty("line.separator")
  
  trait Importable {
    def targetNamespace: Option[String]
    def imports: Seq[String]
    def reader: Reader
    def toSchema(context: Context): Schema
  }
  
  def process(file: File, output: File, packageName: String): List[File] =
    process(file, output,
      Config(packageNames = Map(None -> Some(packageName))))
  
  def process(file: File, output: File, config: Config): List[File] = 
    processFiles(List((file, output)), config)
  
  def processFiles(filePairs: Seq[(File, File)], config: Config): List[File] = {    
    val files = filePairs.map(_._1)
    files.foreach(file => if (!file.exists)
      error("file not found: " + file.toString))
      
    processReaders( (filePairs map { pair =>
        new BufferedReader(new java.io.InputStreamReader(
          new java.io.FileInputStream(pair._1), encoding)) ->
        new PrintWriter(new java.io.OutputStreamWriter(
          new java.io.FileOutputStream(pair._2), encoding)) 
      }).toMap, config)
    val outfiles = ListBuffer.empty[File] ++ (filePairs map { _._2 })
    outfiles foreach { file =>
      println("generated " + file + ".")
    }
    
    if (filePairs.size > 0) {
      val parent = filePairs(0)._2.getParentFile
      val helper = new File(parent, "Helper.scala")
      copyFileFromResource("/Helper.scala", helper)
      outfiles += helper
    }
    
    outfiles.toList
  }
  
  def processReaders(inputToOutput: Map[Reader, PrintWriter], config: Config) = {    
    val context = buildContext
    val importables = inputToOutput.keysIterator.toList map { toImportable(_) }
    val sorted = sortByDependency(importables)
    val schemas = Map[Importable, Schema](sorted map { file =>
      (file, parse(file, context)) }: _*)
    processContext(context, config)
    
    sorted foreach { importable =>
      val schema = schemas(importable)
      val dependents = importable.imports flatMap { i => dependentImportables(sorted, Some(i)).map(schemas(_)) }
      
      val out = inputToOutput(importable.reader)
      try {
        val nodes = generate(schema, dependents, context, config)
        printNodes(nodes, out)
      }
      finally {
        out.flush()
        out.close()
      }
    }
  }
  
  def generate(schema: Schema, dependents: Seq[Schema],
    context: Context, config: Config): Seq[Node]
    
  def toImportable(in: Reader): Importable
  
  def dependentImportables(files: Seq[Importable], namespace: Option[String]) = namespace match {
    case Some(x) => files filter { _.targetNamespace == namespace }
    case _ => Nil
  }
  
  def sortByDependency(files: Seq[Importable]): Seq[Importable] = {        
    val XML_URI = "http://www.w3.org/XML/1998/namespace"    
    val unsorted = ListBuffer.empty[Importable]
    unsorted.appendAll(files)
    val sorted = ListBuffer.empty[Importable]
    val upperlimit = unsorted.size * unsorted.size
        
    for (i <- 0 to upperlimit if unsorted.size > 0) {
      val importable = unsorted(i % unsorted.size)
      if (importable.imports forall { namespace =>
          (namespace == XML_URI) ||
          dependentImportables(files, Some(namespace)).forall(sorted.contains) }) {
        unsorted -= importable
        sorted += importable
      } // if
    }
    
    if (unsorted.size > 0) error("Circular import: " + unsorted.toList)
    sorted
  }
  
  def buildContext: Context
  
  def processContext(context: Context, config: Config): Unit
  
  def parse(importable: Importable, context: Context): Schema
    = importable.toSchema(context)
    
  def parse(in: Reader): Schema
    = parse(toImportable(in), buildContext)
  
  def parse(file: File): Schema
    = parse(new BufferedReader(
      new java.io.InputStreamReader(
        new java.io.FileInputStream(file), encoding)))
    
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
