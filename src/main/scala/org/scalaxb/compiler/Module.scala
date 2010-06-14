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

import org.github.scopt.OptionParser
import collection.mutable.{ListBuffer, ListMap}
import java.io.{File}

trait Logger {
  def log(msg: String) {
    println("["+msg+"]")
    Console.flush
  }
}

trait Module extends Logger {
  type Schema
  type Context
  
  private val config = new ModuleConfig()   
  
  class ModuleConfig {
    var verbose = false
    var packageNames: ListMap[String, Option[String]] =
      ListMap.empty[String, Option[String]]
    var outdir: File = new File(".")  
  }
  
  def start(args: Seq[String]) { 
    val files = ListBuffer.empty[File]
    
    config.packageNames(null) = None
    val paramParser = new OptionParser("scalaxb") {
      opt("d", "outdir", "<directory>", "generated files will go into <directory>",
        { d: String => config.outdir = new File(d) })
      opt("p", "package", "<package>", "specifies the target package",
        { p: String => config.packageNames(null) = Some(p) })
      keyValueOpt("p", "package", "<namespaceURI>", "<package>",
        "specifies the target package for <namespaceURI>",
        { (key: String, value: String) => { config.packageNames(key) = Some(value) } })
      opt("v", "verbose", "be extra verbose",
        { config.verbose = true })
      arglist("<schema_file>...", "input schema to be converted",
        { x: String => files += new File(x) })
    }
    
    if (paramParser.parse(args))
      processFiles(files.map(file =>
          (file, buildOutputFile(file, config.outdir))),
          config.packageNames,
          None
        )
  }
  
  def processFiles(filePairs: Seq[(File, File)],
      packageNames: collection.Map[String, Option[String]],
      verbose: Option[Boolean]) = {
    verbose match {
      case None    =>
      case Some(x) => config.verbose = x
    }
    
    val files = filePairs.map(_._1)
    files.foreach(file => if (!file.exists)
      error("file not found: " + file.toString))
    
    val context = buildContext
    val sorted = sortByDependency(files)
    val schemas = ListMap.empty[File, Schema]
    val outfiles = ListBuffer.empty[File]
    val outputs = ListMap.empty[File, File] ++= filePairs.map(x => x._1 -> x._2)
    val usedPackages = ListBuffer.empty[Option[String]]
        
    for (file <- sorted) 
      schemas += (file -> parse(file, context))
    
    processContext(context, packageNames)
    
    for (file <- sorted) {
      val schema = schemas(file)
      val pkg = packageName(schema, context)
      outfiles += generate(schema, context, outputs(file),
        pkg, !usedPackages.contains(pkg))
      usedPackages += pkg
    }
    
    if (filePairs.size > 0) {
      val parent = filePairs(0)._2.getParentFile
      val helper = new File(parent, "Helper.scala")
      copyFileFromResource("/Helper.scala", helper)
      outfiles += helper
    }
    
    outfiles.toList
  }
  
  def packageName(schema: Schema, context: Context): Option[String]
  
  def process(file: File, output: File, packageName: Option[String],
      verbose: Option[Boolean]) =
    processFiles(List((file, output)),
      Map[String, Option[String]]((null, packageName)), verbose)
  
  def sortByDependency(files: Seq[File]): Seq[File] =
    files

  def buildOutputFile(input: File, outdir: File) = {
    if (!input.exists)
      error("file not found: " + input.toString)
    
    val name = input.getName
    val namepart = name.splitAt(name.indexOf('.'))._1
    new File(outdir, namepart + ".scala") 
  }

  def buildContext: Context
  
  def processContext(context: Context,
      packageNames: collection.Map[String, Option[String]]): Unit
  
  def parse(input: File, context: Context): Schema
  
  def parse(input: File): Schema
    = parse(input, buildContext)
  
  def generate(schema: Schema, context: Context, output: File,
    packageName: Option[String], firstOfPackage: Boolean): File
  
  override def log(msg: String) {
    if (config.verbose) {
      println("["+msg+"]")
      Console.flush
    }
  }
  
  def copyFileFromResource(source: String, dest: File) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.PrintWriter(new java.io.FileWriter(dest))
    var line: String = null
    line = reader.readLine
    while (line != null) {
      out.println(line)
      line = reader.readLine
    }
    in.close
    out.flush
  }
}
