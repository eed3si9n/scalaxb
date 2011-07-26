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

import scalaxb.compiler.{Module, Config, Snippet, CustomXML, CanBeWriter}
import java.io.{File, Reader}
import java.net.{URI}
import collection.mutable
import scala.xml.{Node, Elem}

class Driver extends Module { driver =>
  type Schema = SchemaDecl
  type Context = XsdContext
  type RawSchema = scala.xml.Node
  
  override def buildContext = XsdContext()
    
  override def processContext(context: Context, cnfg: Config) =
    (new ContextProcessor {
      val logger = driver
      val config = cnfg    
    }).processContext(context)

  override def packageName(namespace: Option[String], context: Context): Option[String] =
    (new PackageName {}).packageName(namespace, context)

  override def generate(xsd: Schema, context: Context, cnfg: Config): Snippet =
    (new GenSource(xsd, context) {
      val logger = driver
      val config = cnfg
    }).run
  
  override def generateProtocol(snippet: Snippet,
      context: Context, cnfg: Config): Seq[Node] =
    (new GenProtocol(context) {
      val logger = driver
      val config = cnfg
    }).generateProtocol(snippet)
  
  override def toImportable(alocation: URI, rawschema: RawSchema): Importable = new Importable {
    val location = alocation
    val raw = rawschema
    val schemaLite = SchemaLite.fromXML(raw)
    val targetNamespace = schemaLite.targetNamespace
    val importNamespaces: Seq[String] = schemaLite.imports collect {
      case ImportDecl(Some(namespace: String), _) => namespace
    }
    val importLocations: Seq[String] = schemaLite.imports collect {
      case ImportDecl(_, Some(schemaLocation: String)) => schemaLocation
    }
    val includeLocations: Seq[String] = schemaLite.includes map { _.schemaLocation }
    
    def toSchema(context: Context): Schema = {
      val schema = SchemaDecl.fromXML(raw, context)
      context.schemas += schema
      log("SchemaParser.parse: " + schema.toString())
      schema
    }
  }

  def generateRuntimeFiles[To](cntxt: Context)(implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"))

  def readerToRawSchema(reader: Reader): RawSchema = CustomXML.load(reader)

  def nodeToRawSchema(node: Node) = node
}
