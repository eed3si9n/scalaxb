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
import collection.mutable.ListBuffer
import java.io.{File, FileWriter, PrintWriter}

trait Logger {
  def log(msg: String) {
    println("["+msg+"]")
    Console.flush
  }
}

trait Module extends Logger {
  type Schema
  
  private val config = new ModuleConfig()   
  
  class ModuleConfig {
    var verbose = false
    var packageName: Option[String] = None
    var outdir: File = new File(".")  
  }
  
  def start(args: Seq[String]) { 
    val files = new ListBuffer[java.io.File]
    val paramParser = new OptionParser {
      opt("d", "outdir", "generated files will go into this directory",
        { d: String => config.outdir = new File(d) })
      opt("p", "package", "specifies the target package",
        { p: String => config.packageName = Some(p) })
      opt("v", "verbose", "be extra verbose",
        { config.verbose = true })
      arg("<schema_file>", "input schema to be converted",
        { x: String => files += new File(x) })
    }
    
    if (paramParser.parse(args))
      files.foreach(file => process(file,
        buildOutputFile(file, config.outdir),
        config.packageName))
  }
  
  def buildOutputFile(input: File, outdir: File) = {
    val name = input.getName
    val namepart = name.splitAt(name.indexOf('.'))._1
    new File(outdir, namepart + ".scala") 
  }
  
  def parse(input: File): Schema
  
  def generate(schema: Schema, output: File, packageName: Option[String]): File
    
  def process(input: File, output: File, packageName: Option[String]) = {
    if (!input.exists)
      throw new Exception("file not found: " + input.toString)
    
    val schama = parse(input)
    generate(schama, output, packageName)
  }
  
  override def log(msg: String) {
    if (config.verbose) {
      println("["+msg+"]")
      Console.flush
    }
  }
}
