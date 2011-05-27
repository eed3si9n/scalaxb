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

package scalaxb.compiler.wsdl11

import scala.collection.mutable
import scalaxb.compiler.{Module, Config, Snippet, CustomXML, CanBeWriter}
import scalaxb.{DataRecord}
import wsdl11._
import java.io.{Reader}
import java.net.{URI}
import scala.xml.{Node}
import scalaxb.compiler.xsd.{SchemaLite, SchemaDecl, XsdContext}
import scala.Option._

class Driver extends Module { driver =>
  type Schema = WsdlPair
  type Context = WsdlContext
  type RawSchema = scala.xml.Node

  val xsddriver = new scalaxb.compiler.xsd.Driver {
    override val verbose = driver.verbose
  }

  def buildContext = WsdlContext()

  def readerToRawSchema(reader: Reader): RawSchema = CustomXML.load(reader)

  def nodeToRawSchema(node: Node) = node

  override def packageName(namespace: Option[String], context: Context): Option[String] =
    xsddriver.packageName(namespace, context.xsdcontext)

  override def processContext(context: Context, cnfg: Config) {
    xsddriver.processContext(context.xsdcontext, mod(cnfg))
    context.definitions foreach {processDefinition(_, context)}
  }

  def mod(cnfg: Config) = cnfg.copy(classPrefix = Some("X"))

  def processDefinition(definition: XDefinitionsType, context: Context) {
    val ns = definition.targetNamespace map {_.toString}

    definition.message map { x => context.messages((ns, x.name)) = x }
    definition.portType map { x => context.interfaces((ns, x.name)) = x }
    definition.binding map { x => context.bindings((ns, x.name)) = x }
    definition.service map { x => context.services((ns, x.name)) = x }
  }

  override def generateProtocol(snippet: Snippet,
      context: Context, cnfg: Config): Seq[Node] =
    xsddriver.generateProtocol(snippet, context.xsdcontext, mod(cnfg))

  override def generate(pair: WsdlPair, cntxt: Context, cnfg: Config): Snippet = {
    val ns = (pair.definition, pair.schema) match {
      case (Some(wsdl), _) => wsdl.targetNamespace map {_.toString}
      case (_, Some(schema)) => schema.targetNamespace
      case _ => None
    }

    val generator = new GenSource {
      val logger = driver
      val context = cntxt
      val scope = pair.scope
      val xsdgenerator = new scalaxb.compiler.xsd.GenSource(
        SchemaDecl(targetNamespace = ns, scope = pair.scope),
        cntxt.xsdcontext) {
        val logger = driver
        val config = mod(cnfg)
      }
    }

    val xsdgenerated = pair.schema map {
      xsddriver.generate(_, cntxt.xsdcontext, mod(cnfg))
    } getOrElse { Snippet(<source></source>) }

    val wsdlgenerated = pair.definition map {
      generator.generate(_)
    } getOrElse { Snippet(<source></source>) }

    mergeSnippets(xsdgenerated :: wsdlgenerated :: Nil)
  }

  override def toImportable(alocation: URI, rawschema: RawSchema): Importable = new Importable {
    import scalaxb.compiler.Module.FileExtension
    import scalaxb.compiler.xsd.{ImportDecl}

    log("wsdl11.Driver#toImportable: " + alocation.toString)
    val location = alocation
    val raw = rawschema
    lazy val (wsdl: Option[XDefinitionsType], xsdRawSchema: Option[Node]) = alocation.toString match {
      case FileExtension(".wsdl") =>
        val w = scalaxb.fromXML[XDefinitionsType](rawschema)
        val x = w.types map { _.any.head match {
          case DataRecord(_, _, node: Node) => node
          case _ => error("unexpected type: " + w.types)
        }}
        (Some(w), x)
      case FileExtension(".xsd")  =>
        (None, Some(rawschema))
    }
    lazy val schemaLite = xsdRawSchema map { SchemaLite.fromXML }
    lazy val targetNamespace = (wsdl, schemaLite) match {
      case (Some(wsdl), _) => wsdl.targetNamespace map {_.toString}
      case (_, Some(schemaLite)) => schemaLite.targetNamespace
      case _ => None
    }

    lazy val importNamespaces: Seq[String] =
      (wsdl map { wsdl =>
        wsdl.importValue map {_.namespace.toString}
      } getOrElse {Nil}) ++
      (schemaLite map { schemaLite =>
        schemaLite.imports collect {
         case ImportDecl(Some(namespace: String), _) => namespace
        }} getOrElse {Nil})

    val importLocations: Seq[String] =
      (wsdl map { wsdl =>
        wsdl.importValue map {_.location.toString}
      } getOrElse {Nil}) ++
      (schemaLite map { schemaLite =>
        schemaLite.imports collect {
         case ImportDecl(_, Some(schemaLocation: String)) => schemaLocation
        }} getOrElse {Nil})
    val includeLocations: Seq[String] = schemaLite map { schemaLite =>
      schemaLite.includes map { _.schemaLocation }
    } getOrElse {Nil}

    def toSchema(context: Context): WsdlPair = {
      wsdl foreach { wsdl =>
        log("wsdl11.Driver: " + wsdl.toString)
        context.definitions += wsdl
      }

      val xsd = xsdRawSchema map { x =>
        val schema = SchemaDecl.fromXML(x, context.xsdcontext)
        log("wsdl11.Driver: " + schema.toString)
        context.xsdcontext.schemas += schema
        schema
      }

      WsdlPair(wsdl, xsd, rawschema.scope)
    }
  }

  def generateRuntimeFiles[To](implicit evTo: CanBeWriter[To]): List[To] =
    List(generateFromResource[To](Some("scalaxb"), "scalaxb.scala", "/scalaxb.scala.template"),
      generateFromResource[To](Some("scalaxb"), "soap.scala", "/soap.scala.template"),
      generateFromResource[To](Some("soapenvelope12"), "soapenvelope12.scala", "/soapenvelope12.scala.template"),
      generateFromResource[To](Some("soapenvelope12"), "soapenvelope12_xmlprotocol.scala",
        "/soapenvelope12_xmlprotocol.scala.template"))
}

case class WsdlPair(definition: Option[XDefinitionsType], schema: Option[SchemaDecl], scope: scala.xml.NamespaceBinding)

case class WsdlContext(xsdcontext: XsdContext = XsdContext(),
                       definitions: mutable.ListBuffer[XDefinitionsType] = mutable.ListBuffer(),
                       interfaces:  mutable.ListMap[(Option[String], String), XPortTypeType] = mutable.ListMap(),
                       bindings:    mutable.ListMap[(Option[String], String), XBindingType] = mutable.ListMap(),
                       services:    mutable.ListMap[(Option[String], String), XServiceType] = mutable.ListMap(),
                       faults:      mutable.ListMap[(Option[String], String), XFaultType] = mutable.ListMap(),
                       messages:    mutable.ListMap[(Option[String], String), XMessageType] = mutable.ListMap() )
