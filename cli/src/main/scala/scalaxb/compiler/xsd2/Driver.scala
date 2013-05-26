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
  type Schema = ReferenceSchema
  type Context = SchemaContext
  type RawSchema = scala.xml.Node
  private val logger = Log.forName("xsd2.Driver")
  
  def generate(schema: Schema, part: String, context: Context, config: Config) = {
    val pkg = packageName(schema.targetNamespace map {_.toString}, context)
    Seq((pkg, Snippet(headerSnippet(pkg),
        new Generator(schema, context, config).generateEntitySource), part))
  }

  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node] =
    ProtocolGenerator(context.schemas.head, context, config).generateProtocol(snippet)

  def toImportable(alocation: URI, rawschema: RawSchema): Importable =
    new Importable {
      val raw: RawSchema = rawschema
      val location: URI = alocation

      def schemaNode: Node = (raw \\ "schema").headOption getOrElse {
        sys.error("xsd: schema element not found: " + raw.toString)
      }
      def importNodes: Seq[Node] = schemaNode \ "import"
      def includeNodes: Seq[Node] = schemaNode \ "include"

      def targetNamespace: Option[String] = schemaNode.attribute("targetNamespace").headOption map { _.text }
      def importNamespaces: Seq[String] = importNodes flatMap { node => (node \ "@namespace").headOption.toList map { _.text } }
      def importLocations: Seq[String] = importNodes flatMap { node => (node \ "@schemaLocation").headOption.toList map { _.text } } 
      def includeLocations: Seq[String] = includeNodes flatMap { node => (node \ "@schemaLocation").headOption.toList map { _.text } } 

      def toSchema(context: Context, outerNamespace: Option[String]): Schema = {
        val unbound = fromXML[XSchema](raw)
        // logger.debug("Driver.toSchema: " + unbound.toString())
        val wrapped = ReferenceSchema.fromSchema(replaceTargetNamespace(unbound, outerNamespace), raw.scope)
        logger.debug("Driver.toSchema: " + wrapped.toString)
        wrapped
      }
    }

  private def replaceTargetNamespace(schema: XSchema, tns: Option[String]): XSchema =
    schema.copy(targetNamespace =
      schema.targetNamespace match {
        case Some(x) => Some(x)
        case None    => tns map {new URI(_)} 
      })

  def generateRuntimeFiles[To](cntxt: Context, config: Config)(implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"))

  def buildContext: Context = SchemaContext()

  def packageName(namespace: Option[String], context: Context): Option[String] =
    new PackageNamer {}.packageName(namespace, context)

  def processContext(cntxt: Context, schemas: Seq[Schema], cnfg: Config) {
    cntxt.packageNames ++= cnfg.packageNames
    cntxt.schemas ++= schemas
    schemas.headOption map { s =>
      (new ContextProcessor() with Namer with Lookup with Splitter with Symbols {
        val config = cnfg
        val context = cntxt
        val schema = s
      }).processContext()           
    }
  }

  def processSchema(s: Schema, cntxt: Context, cnfg: Config) =
    (new ContextProcessor() with Namer with Lookup with Splitter with Symbols {
      val config = cnfg
      val context = cntxt
      val schema = s
    }).processSchema(s)

  def readerToRawSchema(reader: Reader): RawSchema = CustomXML.load(reader)

  def nodeToRawSchema(node: Node) = node
  
  def generateRuntimeFiles[To](cntxt: Context)(implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"))  
}
