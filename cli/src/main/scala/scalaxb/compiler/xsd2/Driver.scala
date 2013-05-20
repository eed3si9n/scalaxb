/*
 * Copyright (c) 2011 e.e d3si9n
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

package scalaxb.compiler.xsd2

import java.io.{Reader, PrintWriter}
import java.net.{URI}
import scala.xml.{Node, Elem}
import scalaxb._
import compiler._
import xmlschema._

class Driver extends Module { driver =>
  import com.weiglewilczek.slf4s.Logger
  
  type Schema = ReferenceSchema
  type Context = SchemaContext
  type RawSchema = scala.xml.Node
  override lazy val logger = Logger("xsd2.Driver")
  
  def generate(schema: Schema, context: Context, config: Config): Snippet =
    new Generator(schema, context, config).generateEntitySource

  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node] = Seq(<source/>)

  def toImportable(alocation: URI, rawschema: RawSchema): Importable =
    new Importable {
      val raw: RawSchema = rawschema
      val location: URI = alocation

      def targetNamespace: Option[String] = None
      def importNamespaces: Seq[String] = Nil
      def importLocations: Seq[String] = Nil
      def includeLocations: Seq[String] = Nil

      def toSchema(context: Context): Schema = {
        val unbound = fromXML[XSchema](raw)
        // logger.debug("Driver.toSchema: " + unbound.toString())
        val wrapped = ReferenceSchema.fromSchema(unbound, raw.scope)
        logger.debug("Driver.toSchema: " + wrapped.toString)
        wrapped
      }
    }

  def generateRuntimeFiles[To](implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"))

  def buildContext: Context = SchemaContext()

  def packageName(namespace: Option[String], context: Context): Option[String] =
    new PackageNamer {}.packageName(namespace, context)

  def processSchema(s: Schema, cntxt: Context, cnfg: Config) =
    (new ContextProcessor() with Namer with Lookup with Splitter {
      val config = cnfg
      val context = cntxt
      val schema = s
    }).processSchema(s)

  def processContext(context: Context, config: Config) {
    context.packageNames ++= config.packageNames
  }

  def readerToRawSchema(reader: Reader): RawSchema = CustomXML.load(reader)

  def nodeToRawSchema(node: Node) = node
  
  def generateRuntimeFiles[To](cntxt: Context)(implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"))  
}
