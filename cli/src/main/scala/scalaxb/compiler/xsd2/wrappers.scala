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
 
package scalaxb.compiler.xsd2

import scala.collection.immutable
import scalaxb._
import xmlschema._
import Defs._
import java.net.{URI}
import masked.scalaxb.DataRecord

case class ReferenceSchema(targetNamespace: Option[URI],
                           topElems: immutable.ListMap[String, Tagged[XElement]],
                           topTypes: immutable.ListMap[String, Tagged[XAnnotatedable]],
                           topAttrs: immutable.ListMap[String, TaggedAttr[XAttributable]],
                           topGroups: immutable.ListMap[String, Tagged[XNamedGroup]],
                           topAttrGroups: immutable.ListMap[String, TaggedAttr[XAttributeGroup]],
                           scope: scala.xml.NamespaceBinding,
                           unbound: XSchema)

object ReferenceSchema {
  def fromSchema(schema: XSchema, scope: scala.xml.NamespaceBinding): ReferenceSchema = {
    val ns = schema.targetNamespace
    ReferenceSchema(ns,
      immutable.ListMap[String, Tagged[XElement]](schema.xschemasequence1 collect {
        case XSchemaSequence1(DataRecord(_, _, x: XTopLevelElement), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XAnnotatedable]](schema.xschemasequence1 collect {
        case XSchemaSequence1(DataRecord(_, _, x: XTopLevelSimpleType), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
        case XSchemaSequence1(DataRecord(_, _, x: XTopLevelComplexType), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, TaggedAttr[XAttributable]](schema.xschemasequence1 collect {
        case XSchemaSequence1(DataRecord(_, _, x: XTopLevelAttribute), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XNamedGroup]](schema.xschemasequence1 collect {
        case XSchemaSequence1(DataRecord(_, _, x: XNamedGroup), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, TaggedAttr[XAttributeGroup]](schema.xschemasequence1 collect {
        case XSchemaSequence1(DataRecord(_, _, x: XNamedAttributeGroup), _) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      scope,
      schema)
  }
}

case class WrappedSchema(targetNamespace: Option[String],
                         topElems: Map[String, Tagged[XTopLevelElement]],
                         topTypes: Map[String, Tagged[XAnnotatedable]],
                         topAttrs: Map[String, Tagged[XTopLevelAttribute]],
                         topGroups: Map[String, Tagged[XNamedGroup]],
                         topAttrGroups: Map[String, Tagged[XNamedAttributeGroup]],
                         elemList: List[Tagged[XElement]],
                         typeList: List[Tagged[XAnnotatedable]],
                         choices: List[Tagged[XGroup]],
                         attrList: List[Tagged[XAttributable]],
                         // annotation: Option[AnnotationDecl],
                         scope: scala.xml.NamespaceBinding) {

  val newline = System.getProperty("line.separator")

  override def toString: String = {
    "WrappedSchema(" + newline +
    "topElems(" + topElems.valuesIterator.mkString("," + newline) + ")," + newline +
    "topTypes(" + topTypes.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topAttrs(" + topAttrs.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topGroups(" + topGroups.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topAttrGroups(" + topAttrGroups.valuesIterator.mkString("," + newline)  + ")" + newline +
    ")"
  }
}

object WrappedSchema {
  def typeList(schema: XSchema): Seq[Tagged[XAnnotatedable]] =
    schema.toSeq collect {
      case tagged: TaggedSimpleType  => (tagged: Tagged[XAnnotatedable])
      case tagged: TaggedComplexType => (tagged: Tagged[XAnnotatedable])
    }

  def elemList(schema: XSchema): Seq[Tagged[XElement]] =
    schema.toSeq collect {
      case tagged: TaggedTopLevelElement => (tagged: Tagged[XElement])
      case tagged: TaggedLocalElement    => (tagged: Tagged[XElement])
    }

  def attrList(schema: XSchema): Seq[TaggedAttr[XAttributable]] =
    schema.toSeq collect {
      case tagged: TaggedTopLevelAttribute => tagged
      case tagged: TaggedLocalAttribute    => tagged
    }

  def choiceList(schema: XSchema): Seq[TaggedKeyedGroup] =
    schema.toSeq collect {
      case tagged: TaggedKeyedGroup if tagged.value.key == ChoiceTag => tagged
    }
}
