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

import scalashim._
import scala.collection.mutable
import scalaxb.compiler.{Module, Config, Snippet, CustomXML, CanBeWriter, Log}
import masked.scalaxb.{DataRecord}
import wsdl11._
import java.io.{Reader}
import java.net.{URI}
import scala.xml.{Node}
import scala.reflect.ClassTag
import scalaxb.compiler.xsd.{SchemaLite, SchemaDecl, XsdContext, GenProtocol}

class Driver extends Module { driver =>
  private val logger = Log.forName("wsdl")
  type Schema = WsdlPair
  type Context = WsdlContext
  type RawSchema = scala.xml.Node
  val WSDL_NS = Some("http://schemas.xmlsoap.org/wsdl/")

  val xsddriver = new scalaxb.compiler.xsd.Driver {
    override def verbose = driver.verbose
  }

  def buildContext = WsdlContext()

  def readerToRawSchema(reader: Reader): RawSchema = CustomXML.load(reader)

  def nodeToRawSchema(node: Node) = node

  override def packageName(namespace: Option[String], context: Context): Option[String] =
    xsddriver.packageName(namespace, context.xsdcontext)

  override def processContext(context: Context, schemas: Seq[WsdlPair], cnfg: Config) {
    val xsds = schemas flatMap { _.schemas }
    logger.debug("processContext: " + (xsds map {_.targetNamespace}))
    xsddriver.processContext(context.xsdcontext, xsds, cnfg)
    context.definitions foreach {processDefinition(_, context)}
  }

  def extractChildren[A : ClassTag, B](definition: XDefinitionsType, elementName: String)(f: A => B): Seq[B] =
    definition.xdefinitionstypeoption collect {
      case DataRecord(WSDL_NS, Some(`elementName`), x : A) => f(x)
    }

  override def processSchema(schema: Schema, context: Context, cnfg: Config) {}

  def processDefinition(definition: XDefinitionsType, context: Context) {
    val ns = definition.targetNamespace map {_.toString}

    extractChildren(definition, "message") { x: XMessageType => context.messages((ns, x.name)) = x }
    extractChildren(definition, "portType") { x: XPortTypeType => context.interfaces((ns, x.name)) = x }
    extractChildren(definition, "binding") { x: XBindingType => context.bindings((ns, x.name)) = x }
    extractChildren(definition, "service") { x: XServiceType => context.services((ns, x.name)) = x }
  }

  override def generateProtocol(snippet: Snippet,
      context: Context, cnfg: Config): Seq[Node] =
    (new GenProtocol(context.xsdcontext) {
      val config = cnfg
    }).generateProtocol(snippet, {
      (for {
        dfn <- context.definitions.toList
        tns <- dfn.targetNamespace.toList
      } yield tns.toString).distinct
    })

  def namespaceFromPair(pair: WsdlPair): Option[String] =
    (pair.definition, pair.schemas) match {
      case (Some(wsdl), _) => wsdl.targetNamespace map {_.toString}
      case (_, x :: xs) => x.targetNamespace
      case _ => None
    }    

  override def generate(pair: WsdlPair, part: String, cntxt: Context, cnfg: Config):
      Seq[(Option[String], Snippet, String)] = {
    val ns = namespaceFromPair(pair)
    val generator = new GenSource {
      val config = cnfg
      val context = cntxt
      val scope = pair.scope
      val xsdgenerator = new scalaxb.compiler.xsd.GenSource(
        SchemaDecl(targetNamespace = ns, scope = pair.scope),
        cntxt.xsdcontext) {
        val config = cnfg
      }
    }

    val xsdgenerated: Seq[(Option[String], Snippet, String)] = pair.schemas.zipWithIndex flatMap { case (xsd, i) =>
      xsddriver.generate(xsd, part + "_type" + (i + 1).toString, cntxt.xsdcontext, cnfg)
    }

    val wsdlgenerated: Seq[(Option[String], Snippet, String)] = pair.definition.toList map { wsdl =>
      val pkg = packageName(wsdl.targetNamespace map {_.toString}, cntxt)
      val bindings = extractChildren(wsdl, "binding") { x: XBindingType => x }
      generator.soap11Bindings(bindings) foreach { _ => cntxt.soap11 = true }
      generator.soap12Bindings(bindings) foreach { _ => cntxt.soap12 = true }
      (pkg, Snippet(headerSnippet(pkg), generator.generate(wsdl, bindings)), part)
    }

    xsdgenerated ++ wsdlgenerated
  }

  override def toImportable(alocation: URI, rawschema: RawSchema): Importable = new Importable {
    import scalaxb.compiler.Module.FileExtension
    import scalaxb.compiler.xsd.{ImportDecl}

    logger.debug("toImportable: " + alocation.toString)
    val location = alocation
    val raw = rawschema
    lazy val (wsdl: Option[XDefinitionsType], xsdRawSchema: Seq[Node]) = alocation.toString match {
      case FileExtension(".wsdl") =>
        val w = masked.scalaxb.fromXML[XDefinitionsType](rawschema)
        val x: Seq[Node] = extractChildren(w, "types") { t: XTypesType => t } flatMap { _.any collect {
          case DataRecord(_, _, node: Node) => node
        }}
        (Some(w), x)
      case FileExtension(".xsd")  =>
        (None, List(rawschema))
    }
    lazy val schemaLite = xsdRawSchema map { SchemaLite.fromXML }
    lazy val targetNamespace = (wsdl, schemaLite.toList) match {
      case (Some(wsdl), _) => wsdl.targetNamespace map {_.toString}
      case (_, x :: Nil) => x.targetNamespace
      case _ => None
    }

    lazy val importNamespaces: Seq[String] =
      (wsdl map { wsdl =>
        extractChildren(wsdl, "import") { x: XImportType => x.namespace.toString }
      } getOrElse {Nil}) ++
      (schemaLite flatMap { schemaLite =>
        schemaLite.imports collect {
         case ImportDecl(Some(namespace: String), _) => namespace
        }})

    val importLocations: Seq[String] =
      (wsdl map { wsdl =>
        extractChildren(wsdl, "import") { x: XImportType => x.location.toString }
      } getOrElse {Nil}) ++
      (schemaLite flatMap { schemaLite =>
        schemaLite.imports collect {
         case ImportDecl(_, Some(schemaLocation: String)) => schemaLocation
        }})
    val includeLocations: Seq[String] = schemaLite flatMap { schemaLite =>
      schemaLite.includes map { _.schemaLocation }
    }

    def toSchema(context: Context): WsdlPair = {
      wsdl foreach { wsdl =>
        logger.debug(wsdl.toString)
        context.definitions += wsdl
      }

      val xsd = xsdRawSchema map { x =>
        val schema = SchemaDecl.fromXML(x, context.xsdcontext)
        logger.debug(schema.toString)
        schema
      }

      WsdlPair(wsdl, xsd, rawschema.scope)
    }
    def swapTargetNamespace(outerNamespace: Option[String], n: Int): Importable =
      wsdl match {
        case Some(_) => toImportable(alocation, rawschema)
        case None    => toImportable(appendPostFix(alocation, n), replaceNamespace(rawschema, targetNamespace, outerNamespace))
      }
  }

  val VersionPattern = """(\d+)\.(\d+)\.(\d+)""".r
  def generateRuntimeFiles[To](cntxt: Context, config: Config)(implicit evTo: CanBeWriter[To]): List[To] =
    List(
      generateFromResource[To](Some("scalaxb"), "scalaxb.scala",
        "/scalaxb.scala.template"),
      (if (config.async)
        generateFromResource[To](Some("scalaxb"), "httpclients_async.scala",
          "/httpclients_async.scala.template")
      else
        generateFromResource[To](Some("scalaxb"), "httpclients.scala",
          "/httpclients.scala.template"))) ++
    List((config.dispatchVersion, config.async) match {
      case (VersionPattern(x, y, z), false) if (x.toInt == 0) && (y.toInt < 10) =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch.scala",
          "/httpclients_dispatch_classic.scala.template")
      case (VersionPattern(x, y, z), true) if (x.toInt == 0) && (y.toInt < 10) =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch_async.scala",
          "/httpclients_dispatch_classic_async.scala.template")

      case (VersionPattern(x, y, z), false) if (x.toInt == 0) &&
          ((y.toInt == 10) || ((y.toInt == 11) && (z.toInt == 0))) =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch.scala",
          "/httpclients_dispatch0100.scala.template")
      case (VersionPattern(x, y, z), true) if (x.toInt == 0) &&
          ((y.toInt == 10) || ((y.toInt == 11) && (z.toInt == 0))) =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch_async.scala",
          "/httpclients_dispatch0100_async.scala.template")

      case (_, false)  =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch.scala",
          "/httpclients_dispatch0111.scala.template")
      case (_, true)  =>
        generateFromResource[To](Some("scalaxb"), "httpclients_dispatch_async.scala",
           "/httpclients_dispatch0111_async.scala.template")
    }) ++
    (if (cntxt.soap11) List(
      (if (config.async)
        generateFromResource[To](Some("scalaxb"), "soap11_async.scala",
          "/soap11_async.scala.template")
      else
        generateFromResource[To](Some("scalaxb"), "soap11.scala",
          "/soap11.scala.template")),
      generateFromResource[To](Some("soapenvelope11"), "soapenvelope11.scala",
        "/soapenvelope11.scala.template"),
      generateFromResource[To](Some("soapenvelope11"), "soapenvelope11_xmlprotocol.scala",
        "/soapenvelope11_xmlprotocol.scala.template"))
    else Nil) ++
    (if (cntxt.soap12) List(
      (if (config.async)
        generateFromResource[To](Some("scalaxb"), "soap12_async.scala",
          "/soap12_async.scala.template")
      else
        generateFromResource[To](Some("scalaxb"), "soap12.scala",
          "/soap12.scala.template")),
      generateFromResource[To](Some("soapenvelope12"), "soapenvelope12.scala",
        "/soapenvelope12.scala.template"),
      generateFromResource[To](Some("soapenvelope12"), "soapenvelope12_xmlprotocol.scala",
        "/soapenvelope12_xmlprotocol.scala.template"))
    else Nil)
}

case class WsdlPair(definition: Option[XDefinitionsType], schemas: Seq[SchemaDecl], scope: scala.xml.NamespaceBinding)

case class WsdlContext(xsdcontext: XsdContext = XsdContext(),
                       definitions: mutable.ListBuffer[XDefinitionsType] = mutable.ListBuffer(),
                       interfaces:  mutable.ListMap[(Option[String], String), XPortTypeType] = mutable.ListMap(),
                       bindings:    mutable.ListMap[(Option[String], String), XBindingType] = mutable.ListMap(),
                       services:    mutable.ListMap[(Option[String], String), XServiceType] = mutable.ListMap(),
                       faults:      mutable.ListMap[(Option[String], String), XFaultType] = mutable.ListMap(),
                       messages:    mutable.ListMap[(Option[String], String), XMessageType] = mutable.ListMap(),
                       var soap11:  Boolean = false,
                       var soap12:  Boolean = false)
