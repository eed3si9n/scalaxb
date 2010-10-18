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

case class Config(packageNames: Map[Option[String], Option[String]] = Map(None -> None),
  classPrefix: Option[String] = None,
  paramPrefix: Option[String] = None,
  wrappedComplexTypes: List[String] = Nil)

trait Module extends Logger {
  type Schema
  type Context
  
  def verbose: Boolean = false
  
  val encoding = "UTF-8"
  
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
    val outfiles = ListBuffer.empty[File] ++ (filePairs map { pair =>
      pair._2
    })
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
    val importables = inputToOutput.keysIterator.toList map {
      toImportable(_)
    }
    
    val sorted = sortByDependency(importables)
    val schemas = ListMap.empty[Importable, Schema]
    
    sorted foreach { importable =>
      schemas(importable) = parse(importable, context)
    }
    
    processContext(context, config)
    
    sorted foreach { importable =>
      val schema = schemas(importable)
      val pkg = packageName(schema, context)
      
      val out = inputToOutput(importable.reader)
      try {
        generate(schema, context, out, pkg, config)
      }
      finally {
        out.flush()
        out.close()
      }
    }
  }
  
  def toImportable(in: Reader): Importable
  
  def packageName(schema: Schema, context: Context): Option[String]
  
  def sortByDependency(files: Seq[Importable]): Seq[Importable] = {
    val schemaFiles = ListMap.empty[Option[String], Importable]

    files foreach { importable =>
      importable.targetNamespace foreach { targetNamespace =>
        schemaFiles += (importable.targetNamespace -> importable) 
      } 
    }

    val XML_URI = "http://www.w3.org/XML/1998/namespace"    
    val unsorted = ListBuffer.empty[Importable]
    unsorted.appendAll(files)
    val sorted = ListBuffer.empty[Importable]
    val upperlimit = unsorted.size * unsorted.size
    
    def containsAll(imports: Seq[String]) = imports.forall { namespace =>
      (namespace == XML_URI) || sorted.contains(schemaFiles(Some(namespace)))
    }

    for (i <- 0 to upperlimit) {
      if (unsorted.size > 0) {
        val importable = unsorted(i % unsorted.size)
        if ((importable.imports.size == 0) ||
            containsAll(importable.imports)) {
          unsorted -= importable
          sorted += importable
        } // if
      } // if
    }

    if (unsorted.size > 0)
      error("Circular import: " + unsorted.toList)
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
  
  def generate(schema: Schema, context: Context, output: PrintWriter,
    packageName: Option[String], config: Config): PrintWriter
  
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
