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

import scalaxb.compiler.{Module, Config, Snippet, Log}
import scala.xml._

class GenProtocol(val context: XsdContext, var config: Config) extends ContextProcessor {
  private val logger = Log.forName("xsd.GenProtocol")

  private def buildHelper(clauses: Seq[Node], chunkLength: Int, index: Int, helperBody: Seq[Node], headBody: Seq[String], tailBody: Seq[String]): (Seq[Node], String) =
    clauses.splitAt(chunkLength) match {
      case (Nil, _) =>
        (helperBody.reverse, (headBody.reverse ++: tailBody).mkString)

      case (blockClauses, moreClauses) =>
        val newHelperBody = <source>  def fromAnySchemaTypeHelper{index}(elem: scala.xml.Elem): Option[scalaxb.DataRecord[Any]] = {{
    import scalaxb.{{Helper, DataRecord, fromXML}}

    val ns = Helper.nullOrEmpty(elem.scope.getURI(elem.prefix))
    val key = Some(elem.label)
    val (xsns, xstype) = Helper.instanceType(elem)

    (key, ns) match {{
{blockClauses}
      case _ => None
    }}
  }}</source> +: helperBody
        val newHeadBody = if (index > 1) {
          (" match {" + newline + indent(index + 1) + s"case None => fromAnySchemaTypeHelper$index(elem)") +: headBody
        } else {
          Seq(indent(2) + "fromAnySchemaTypeHelper1(elem)")
        }
        val newTailBody = if (index > 1) {
          (newline + indent(index + 1) + "case result => result" +
           newline + indent(index) + "}") +: tailBody
        } else {
          Nil
        }
        buildHelper(moreClauses, chunkLength, index + 1, newHelperBody, newHeadBody, newTailBody)
    }

  def generateProtocol(snippet: Snippet, serviceTargetNamespaces: Seq[String]): Seq[Node] = {

    val name = makeTypeName("XMLProtocol")
    val scopeSchemas = context.schemas
    def makeScope(x: SchemaDecl): Option[(Option[String], String)] = x.targetNamespace match {
      case Some(ns) =>
        val prefix = makePrefix(x.targetNamespace, context)
        if (prefix == "") Some(Some("tns"), ns)
        else Some(Some(prefix), ns)
      case _ => None
    }

    def makeDistinct(list: List[(Option[String], String)], counter: Int): List[(Option[String], String)] = {
      def sortOption(a: (Option[String], String), b: (Option[String], String)) = {
        (a._1, b._1) match {
          case (None, _) => true
          case (_, None) => false
          case (a, b) => a.get < b.get
        }
      }

      val sortedList = list.sortWith((a, b) => sortOption(a, b))
      sortedList match {
        case x :: l if x._1.isEmpty => { x :: makeDistinct(l, counter) }
        case x :: l if !l.isEmpty && x._1.get == l.head._1.get => { (x._1.map(_ + counter.toString), x._2) :: makeDistinct(l, counter + 1) }
        case x :: l => { x :: makeDistinct(l, counter) }
        case _ => Nil
      }
    }

    // including XS_URL into the default scope prints out the xsi:type, which is necessary for anyType round trip.
    val scopes = makeDistinct((
      (config.defaultNamespace match {
        case Some(ns) if scopeSchemas.toList forall {_.elementQualifiedDefault} => List(None -> ns)
        case _ => Nil }) :::
      (scopeSchemas.toList flatMap {makeScope _}) :::
      (serviceTargetNamespaces.toList.zipWithIndex map { case (ns, idx) => Some("tns") -> ns }) :::
      List((Some(XSI_PREFIX) -> XSI_URL), (Some(XS_PREFIX) -> XS_URL))).distinct, 0)
    val packageString = config.protocolPackageName map { "package " + _ + newline } getOrElse{""}
    val importFutureString = if (config.async)
      "import scala.concurrent.{ Future, ExecutionContext }" + newline else ""
    val packageValueString = config.protocolPackageName map { x => x } getOrElse {""}
    val maxChunkLength = 200
    val nOfClauses = snippet.elemToTypeClauses.length
    val (fromAnySchemaTypeHelper, fromAnySchemaTypeHelperCase) =
      if (nOfClauses > maxChunkLength) {
        val nOfChunks: Int = (nOfClauses + maxChunkLength - 1) / maxChunkLength
        val chunkLength: Int = (nOfClauses + nOfChunks - 1) / nOfChunks
        buildHelper(snippet.elemToTypeClauses, chunkLength, 1, Nil, Nil, Nil)
      } else {
        (Nil, <source>    import scalaxb.{{Helper, DataRecord, fromXML}}

    val ns = Helper.nullOrEmpty(elem.scope.getURI(elem.prefix))
    val key = Some(elem.label)
    val (xsns, xstype) = Helper.instanceType(elem)

    (key, ns) match {{
{snippet.elemToTypeClauses}
      case _ => None
    }}</source>)
      }

    <source>// Generated by &lt;a href="http://scalaxb.org/"&gt;scalaxb&lt;/a&gt;.
{packageString}
{importFutureString}

/**
usage:
val obj = scalaxb.fromXML[{packageValueString}.Foo](node)
val document = scalaxb.toXML[{packageValueString}.Foo](obj, "foo", {packageValueString}.defaultScope)
**/
object `package` extends { buildDefaultProtocolName(name) } {{ }}

trait { buildDefaultProtocolName(name) } extends scalaxb.XMLStandardTypes {{
  val defaultScope = scalaxb.toScope({ if (scopes.isEmpty) "Nil: _*"
    else scopes.map(x => quote(x._1) + " -> " + quote(x._2)).mkString("," + newline + indent(2)) })
{snippet.implicitValue}
{fromAnySchemaTypeHelper}
  implicit val fromAnySchemaType: scala.xml.Elem => Option[scalaxb.DataRecord[Any]] = {{elem =>
{fromAnySchemaTypeHelperCase}
  }}

{snippet.defaultFormats}
}}</source>
  }

  def buildDefaultProtocolName(name: String): String =
    config.classPrefix map { p => p + name.drop(p.length) } getOrElse {name}
}
