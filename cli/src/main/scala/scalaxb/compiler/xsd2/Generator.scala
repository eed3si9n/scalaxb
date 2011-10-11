/*
 * Copyright (c) 2010-2011 e.e d3si9n
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

import scala.xml.Node
import scalaxb.compiler.{Config, Snippet}
import xmlschema._
import Defs._

class Generator(val schema: ReferenceSchema, 
    val context: SchemaContext, val config: Config) extends Params with PackageNamer
    with Namer with Lookup with Splitter {
  import com.weiglewilczek.slf4s.{Logger}
  private lazy val logger = Logger("xsd2.Generator")
  
  def generateEntitySource: Snippet =
    Snippet(
      headerSnippet +:
      (schema.unbound.toSeq flatMap {
        case x: TaggedComplexType => processComplexType(x)
        case x: TaggedSimpleType if containsEnumeration(x) && isRootEnumeration(x) => processSimpleType(x)
        case _ => Nil
      }): _*)

  def processComplexType(decl: Tagged[XComplexType]): List[Snippet] =
    List(generateComplexTypeEntity(buildTypeName(decl), decl))

  def generateComplexTypeEntity(name: QualifiedName, decl: Tagged[XComplexType]) = {
    logger.debug("generateComplexTypeEntity: emitting %s" format name.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)
    lazy val allRef: Tagged[AllParam] = Tagged(AllParam(), decl.tag)

    logger.debug("generateComplexTypeEntity: decl: %s" format decl.toString)

    val localName = name.localPart
    val attributes = decl.flattenAttributes
    val list =
      (decl.primaryAll map { _ => Seq(allRef) } getOrElse {splitParticlesIfLong(decl.particles)(decl.tag)}) ++
      (attributes.headOption map { _ => attributeSeqRef }).toSeq
    val paramList: Seq[Param] = Param.fromSeq(list)
    val compositors =  decl.compositors flatMap {splitIfLongSequence} filterNot {
      Some(_) == decl.primarySequence &&
      (decl.primarySequence map { tagged => Occurrence(tagged.value).isSingle } getOrElse {true}) }
    val compositorCodes = compositors.toList map {generateCompositor}
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.occurrence.isMultiple) &&
          (!paramList.head.attribute) && (!decl.mixed) // && (!longAll)
    val paramsString =
      if (hasSequenceParam) makeParamName(paramList.head.name) + ": " + paramList.head.singleTypeName + "*"
      else paramList.map(_.toScalaCode).mkString(", " + NL + indent(1))
    val accessors =
      (decl.primaryAll map { generateAllAccessors(_) } getOrElse {
        splitParticles(decl.particles)(decl.tag) map { generateLongSeqAccessors(_) } getOrElse {Nil} }) ++
      (attributes.headOption map  { _ => generateAccessors(attributes) } getOrElse {Nil})

    Snippet(Snippet(<source>case class { localName }({paramsString}){ accessors.headOption map( _ =>
      " {" + NL + indent(1) + accessors.mkString(NL + indent(1)) + NL + "}" + NL
    ) getOrElse("") }</source>) :: compositorCodes: _*)
  }

  def generateSequence(tagged: Tagged[KeyedGroup]): Snippet = {
    implicit val tag = tagged.tag
    val name = names.get(tagged) getOrElse {"??"}
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
    val list = splitParticlesIfLong(tagged.particles)
    val paramList = Param.fromSeq(list)
    Snippet(<source>case class { name }({
      paramList.map(_.toScalaCode).mkString(", " + NL + indent(1))})</source>)
  }

  def generateCompositor(decl: Tagged[KeyedGroup]): Snippet = decl.key match {
    case "sequence" => generateSequence(decl)
    case _ =>
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
      val superString = ""
      val name = names.get(decl) getOrElse {"??"}
      Snippet(<source>trait {name}{superString}</source>)
  }

  def processSimpleType(decl: Tagged[XSimpleType]): List[Snippet] =
    List(generateSimpleType(buildTypeName(decl), decl))

  def generateSimpleType(name: QualifiedName, decl: Tagged[XSimpleType]) = {
    val localName = name.localPart
    val enumValues = filterEnumeration(decl) map { enum =>
      val name = buildTypeName(enum)
      """case object %s extends %s { override def toString = "%s" }""".format(
        name.localPart, localName, enum.value.value)
    }

    Snippet(<source>trait { localName }
{ enumValues.mkString(NL) }</source>)
  }

  def generateAllAccessors(tagged: Tagged[KeyedGroup]): Seq[String] = {
    implicit val tag = tagged.tag
    val paramList = Param.fromSeq(tagged.particles)
    paramList map {_.toDataRecordMapAccessor(makeParamName(ALL_PARAM))}
  }

  def generateLongSeqAccessors(splits: Seq[TaggedKeyedGroup]): Seq[String] =
    splits flatMap { sequence =>
      implicit val tag = sequence.tag
      val wrapper = Param.fromSeq(Seq(sequence)).head
      val paramList = Param.fromSeq(sequence.particles)
      paramList map { _.toLongSeqAccessor(wrapper.paramName) }
    }

  def generateAccessors(attributes: Seq[Tagged[_]]): Seq[String] =
    Param.fromAttributes(attributes) map {_.toDataRecordMapAccessor(makeParamName(ATTRS_PARAM))}

  def headerSnippet: Snippet =
    Snippet(<source>// Generated by scalaxb.</source> +: packageCode, Nil, Nil, Nil)

  def packageCode: Seq[Node] =
    (packageNameByURI(schema.targetNamespace, context) map { pkg =>
      <source>package {pkg}</source>
    }).toSeq
}
