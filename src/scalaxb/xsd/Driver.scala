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
 
package scalaxb.xsd

import scalaxb.{Module, Main}
import scala.collection.Map
import java.io.{File, FileWriter, PrintWriter}

object Driver extends Module {
  type Config = XsdConfig
  type Schema = SchemaDecl
  
  class XsdConfig extends super.ModuleConfig {

  }
    
  def generate(xsd: Schema, output: File) {
    val out = new PrintWriter(new FileWriter(output))
    Main.log("xsd: generating ...")
    new GenSource(config, xsd, out) run;
    out.flush()
    out.close()
    println("generated " + output)
  }

  lazy val config = new XsdConfig()       

  def parse(input: java.io.File): Schema = {
    Main.log("xsd: parsing " + input)
    val elem = scala.xml.XML.loadFile(input)
    val schema = SchemaDecl.fromXML(elem)
    Main.log("SchemaParser.parse: " + schema.toString())
    schema
  }
  
  def verifySchema(sd: Schema): Boolean = 
    true
  
  def verifyOutput(): Boolean =
    true
}
