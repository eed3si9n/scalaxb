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
 
package scalaxb.compiler.xsd

import scalaxb.compiler.{Module, Config, Snippet}
import java.io.{File, Reader, PrintWriter}
import collection.mutable
import scala.xml.{Node, Elem}
import scala.xml.factory.{XMLLoader}
import javax.xml.parsers.SAXParser

class Driver extends Module { driver =>
  type Schema = SchemaDecl
  type Context = XsdContext
  
  override def buildContext = XsdContext()
    
  override def processContext(context: Context, cnfg: Config) =
    (new ContextProcessor {
      val logger = driver
      val config = cnfg    
    }).processContext(context)
  
  override def generate(xsd: Schema, dependents: Seq[Schema],
      context: Context, cnfg: Config): Snippet =
    (new GenSource(xsd, dependents, context) {
      val logger = driver
      val config = cnfg
    }).run
  
  override def generateProtocol(snippet: Snippet,
      context: Context, cnfg: Config): Seq[Node] =
    (new GenProtocol(context) {
      val logger = driver
      val config = cnfg
    }).generateProtocol(snippet)
  
  override def toImportable(in: Reader): Importable = new Importable {
    val reader = in
    val elem = CustomXML.load(reader)
    val schemaLite = SchemaLite.fromXML(elem)
    val targetNamespace = schemaLite.targetNamespace
    val imports: Seq[String] = schemaLite.imports.collect {
      case ImportDecl(Some(namespace: String), _) => namespace
    }
    
    def toSchema(context: Context): Schema = {
      val schema = SchemaDecl.fromXML(elem, context)
      context.schemas += schema
      log("SchemaParser.parse: " + schema.toString())
      schema
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
}
