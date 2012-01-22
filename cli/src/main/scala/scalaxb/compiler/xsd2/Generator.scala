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
import scalaxb.compiler.{Config, Snippet, Trippet}
import xmlschema._
import Defs._

class Generator(val schema: ReferenceSchema, 
    val context: SchemaContext, val config: Config) extends Params with PackageNamer
    with Namer with Lookup with Splitter with Parsers with Args with XMLOutputs {
  import Predef.{any2stringadd => _}
  import com.weiglewilczek.slf4s.Logger
  import scalaxb.DataRecord
  import treehugger._
  import definitions._
  import treehuggerDSL._

  private lazy val logger = Logger("xsd2.Generator")
  
  def generateEntitySource: Snippet =
    Snippet(
      headerSnippet +:
      (schema.unbound.toSeq flatMap {
        case x: TaggedComplexType => processComplexType(x) map {_.toSnippet}
        case x: TaggedSimpleType if containsEnumeration(x) && isRootEnumeration(x) => processSimpleType(x)
        case x@TaggedAttributeGroup(group: XNamedAttributeGroup, _) => processAttributeGroup(x) map {_.toSnippet}
        case _ => Nil
      }): _*)

  def processComplexType(decl: Tagged[XComplexType]): Seq[Trippet] =
    Seq(generateComplexTypeEntity(buildTypeName(decl), decl))

  def generateComplexTypeEntity(name: QualifiedName, decl: Tagged[XComplexType]): Trippet = {
    logger.debug("generateComplexTypeEntity: emitting %s" format name.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)
    lazy val allRef: Tagged[AllParam] = Tagged(AllParam(), decl.tag)

    logger.debug("generateComplexTypeEntity: decl: %s" format decl.toString)

    val localName = name.localPart
    val attributes = decl.flattenedAttributes
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
    val paramsTrees =
      if (hasSequenceParam) paramList.head.varargTree :: Nil
      else paramList map {_.tree}

    val accessors: Seq[Tree] =
      (decl.primaryAll map { generateAllAccessors(_) } getOrElse {
        splitParticles(decl.particles)(decl.tag) map { generateLongSeqAccessors(_) } getOrElse {Nil} }) ++
      (attributes.headOption map  { _ => generateAttributeAccessors(attributes, true) } getOrElse {Nil})
    val superNames = complexTypeSuperNames(decl)
    val parents = superNames map { x => RootClass.newClass(x.toTypeName).toType}

//    Snippet(Snippet(<source>case class { localName }({paramsString}){extendString}{ accessors.headOption map( _ =>
//      " {" + NL + indent(1) + accessors.mkString(NL + indent(1)) + NL + "}" + NL
//    ) getOrElse("") }</source>, <source/>, generateDefaultFormat(name, decl),
//      makeImplicitValue(name)) :: compositorCodes: _*)

    Trippet(
      Trippet(CASECLASSDEF(localName.toTypeName) withParams(paramsTrees: _*) withParents(parents: _*) :=
        (if (accessors.isEmpty) forest.EmptyTree
        else BLOCK(accessors: _*))
      ) ::
      compositorCodes: _*)
  }

  private def generateDefaultFormat(name: QualifiedName, decl: Tagged[XComplexType]): Node = {
    val particles = decl.particles
    val unmixedParserList = particles map { buildParser(_, decl.mixed, decl.mixed) }
    val parserList = if (decl.mixed) buildTextParser +: (unmixedParserList flatMap { Seq(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList = ( 0 to parserList.size - 1) map { buildSelector }

    val particleArgs = if (decl.mixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(particles((i - 1) / 2), i)
        else buildArgForOptTextRecord(i) }
      else decl.primaryAll map { all => 
        implicit val tag = all.tag
        all.particles map { buildArgForAll(_) } } getOrElse {
        particles.zipWithIndex map { case (i, x) => buildArg(i, x) }
      }
    val argsString = particleArgs.mkString("," + NL + indent(4))

    def makeWritesChildNodes = {
      def simpleContentString(base: QualifiedName) = base match {
        case BuiltInAnyType(_) => "Seq(scala.xml.Text(__obj.value.value.toString))"
        case _ => "Seq(scala.xml.Text(__obj.value.toString))"
      }

      def childString = if (decl.mixed) "__obj." + makeParamName(MIXED_PARAM) +
        ".toSeq flatMap { x => " + buildToXML(QualifiedName.DataRecordAnyTypeName, "x, x.namespace, x.key, __scope, false") + " }"
      else decl.value.arg1.value match {
        case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => simpleContentString(x.base)
        case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => simpleContentString(x.base)
        case _ =>
          if (particles.isEmpty) "Nil"
          else if (particles.size == 1) "(" + buildXMLString(Param(particles(0))) + ")"
          else (Param.fromSeq(particles) map { x => buildXMLString(x) }).mkString("Seq.concat(", "," + NL + indent(4), ")")
      }

      <source>    def writesChildNodes(__obj: {name.fullyQualifiedName}, __scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node] =
            {childString}</source>
    }

    val makeWritesAttribute = ""


    val groups = decl.flattenedGroups filter { case tagged: TaggedKeyedGroup =>
      implicit val tag = tagged.tag
      tagged.particles.size > 0 }
    val defaultFormatSuperNames: Seq[String] = ("scalaxb.ElemNameParser[" + name.fullyQualifiedName + "]") ::
      (groups.toList map { case tagged: TaggedKeyedGroup => QualifiedName(tagged.tag.namespace, names.get(tagged).get).formatterName })

    <source>  trait Default{name.formatterName} extends {defaultFormatSuperNames.mkString(" with ")} {{
    val targetNamespace: Option[String] = { quoteUri(schema.targetNamespace) }

    { if (decl.name.isDefined) "override def typeName: Option[String] = " + quote(decl.name) + NL + NL + indent(2)
      else ""
    }{ if (decl.mixed) "override def isMixed: Boolean = true" + NL + NL + indent(2)
       else "" }def parser(node: scala.xml.Node, stack: List[scalaxb.ElemName]): Parser[{name.fullyQualifiedName}] =
      { parserList.mkString(" ~ " + NL + indent(3)) } ^^
      {{ case { parserVariableList.mkString(" ~ ") } =>
      {name.fullyQualifiedName}({argsString}) }}

{makeWritesAttribute}{makeWritesChildNodes}  }}</source>
  }

  private def makeImplicitValue(name: QualifiedName): Node =
    <source>  implicit lazy val {name.formatterName}: scalaxb.XMLFormat[{name.fullyQualifiedName}] = new Default{name.formatterName} {{}}</source>

  def complexTypeSuperNames(decl: Tagged[XComplexType]): Seq[String] = {
    (decl.attributeGroups map {buildTypeName} map {_.localPart})
  }

  def generateSequence(tagged: Tagged[KeyedGroup]): Trippet = {
    implicit val tag = tagged.tag
    val name = names.get(tagged) getOrElse {"??"}
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
    val list = splitParticlesIfLong(tagged.particles)
    val paramList = Param.fromSeq(list)
    // Snippet(<source>case class { name }({
    //  paramList.map(_.toScalaCode).mkString(", " + NL + indent(1))})</source>)
    Trippet(CASECLASSDEF(name.toTypeName) withParams(paramList map {_.tree}: _*))
  }

  def generateCompositor(decl: Tagged[KeyedGroup]): Trippet = decl.key match {
    case "sequence" => generateSequence(decl)
    case _ =>
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
      val superString = ""
      val name = names.get(decl) getOrElse {"??"}
      // Snippet(<source>trait {name}{superString}</source>)
      Trippet(TRAITDEF(name.toTypeName))
  }

  def processSimpleType(decl: Tagged[XSimpleType]): Seq[Snippet] =
    Seq(generateSimpleType(buildTypeName(decl), decl))

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
  
  def processAttributeGroup(tagged: Tagged[XAttributeGroup]): Seq[Trippet] =
    Seq(generateAttributeGroup(buildTypeName(tagged), tagged))

  def generateAttributeGroup(name: QualifiedName, tagged: Tagged[XAttributeGroup]): Trippet = {
    val localName = name.localPart
    val accessors = generateAttributeAccessors(tagged.flattenedAttributes, false)
    Trippet(TRAITDEF(localName) := BLOCK(accessors: _*))
  }

  def generateAllAccessors(tagged: Tagged[KeyedGroup]): Seq[Tree] = {
    implicit val tag = tagged.tag
    val paramList = Param.fromSeq(tagged.particles)
    paramList map {_.toDataRecordMapAccessor(makeParamName(ALL_PARAM), true)}
  }

  def generateLongSeqAccessors(splits: Seq[TaggedKeyedGroup]): Seq[Tree] =
    splits flatMap { sequence =>
      implicit val tag = sequence.tag
      val wrapper = Param.fromSeq(Seq(sequence)).head
      val paramList = Param.fromSeq(sequence.particles)
      paramList map { _.toLongSeqAccessor(wrapper.paramName) }
    }

  def generateAttributeAccessors(attributes: Seq[Tagged[_]], generateImpl: Boolean): Seq[Tree] =
    Param.fromAttributes(attributes) map {_.toDataRecordMapAccessor(makeParamName(ATTRS_PARAM), generateImpl)}

  def headerSnippet: Snippet =
    Snippet(<source>// Generated by scalaxb.</source> +: packageCode, Nil, Nil, Nil)

  def packageCode: Seq[Node] =
    (packageNameByURI(schema.targetNamespace, context) map { pkg =>
      <source>package {pkg}</source>
    }).toSeq
}
