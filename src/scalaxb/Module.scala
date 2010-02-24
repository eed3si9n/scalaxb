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
 
package scalaxb

import org.github.scopt.OptionParser
import collection.mutable.ListBuffer
import java.io.{File, FileWriter, PrintWriter}

trait Module {
  type Config <: ModuleConfig
  type Schema
  
  class ModuleConfig {
    var args: Seq[String] = Nil
    var verbose = false
    var packageName: String = _
    var doVerifySchema = false
    var doVerifyOutput = false
    var outdir: File = _
  }
  
  def start() {    
    val files = new ListBuffer[java.io.File]
    val paramParser = new OptionParser {
      opt("d", "outdir", "generated files will go into this directory",
        { d: String => config.outdir = new java.io.File(d) })
      opt("p", "package", "specifies the target package",
        { p: String => config.packageName = p })
      opt("v", "verbose", "be extra verbose",
        { config.verbose = true })
      arg("<schema_file>", "input schema to be converted",
        { x: String => files += new java.io.File(x) })
    }
    
    if (paramParser.parse(config.args))
      for (file <- files)
        if (file.exists)
          process(file)
        else
          throw new Exception("file not found: " + file.toString)
  }
  
  def parse(input: File): Schema
  
  def generate(schema: Schema, output: File): Unit
  
  def config: Config
  
  def process(input: File) {
    val schama = parse(input)
    var sdv = !(config.doVerifySchema) || verifySchema(schama);
    generate(schama, buildOutputFile(input))
    var outv = !(config.doVerifyOutput) || verifyOutput();
  }
  
  def buildOutputFile(input: File) = {
    val dir = if (config.outdir == null)
      new File(".")
    else
      config.outdir
    val name = input.getName
    val namepart = name.splitAt(name.indexOf('.'))._1
    new File(dir, namepart + ".scala") 
  }
  
  def verifySchema(sd: Schema): Boolean
  
  def verifyOutput(): Boolean
}
