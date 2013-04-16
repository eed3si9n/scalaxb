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
    with Namer with Lookup with Splitter with Parsers with Args with XMLOutputs with Symbols {
  import Predef.{any2stringadd => _}
  import com.codahale.logula.Log
  import scalaxb.DataRecord
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private val logger = Log.forName("xsd2.Generator")
  
  def generateEntitySource: Snippet =
    Snippet(
      (schema.unbound.toSeq flatMap {
        case x: TaggedComplexType => processComplexType(x) map {_.toSnippet}
        case x: TaggedSimpleType if containsEnumeration(x) && isRootEnumeration(x) => processSimpleType(x) map {_.toSnippet}
        case x@TaggedAttributeGroup(group: XNamedAttributeGroup, _) => processAttributeGroup(x) map {_.toSnippet}
        case _ => Nil
      }): _*)

  def processComplexType(decl: Tagged[XComplexType]): Seq[Trippet] =
    Seq(generateComplexTypeEntity(buildComplexTypeSymbol(decl), decl))

  def compositorsR(decl: Tagged[XComplexType]): Seq[TaggedParticle[KeyedGroup]] = {
    val ps = decl.primarySequence
    val singleps = ps map { tagged => Occurrence(tagged.value).isSingle } getOrElse {false}

    def compositorsR(compositor: TaggedParticle[KeyedGroup]): Seq[TaggedParticle[KeyedGroup]] =
      (compositor match {
        case x: TaggedKeyedGroup if x.value.key == ChoiceTag => Seq(x)
        case x: TaggedKeyedGroup if x.value.key == AllTag => Seq(x)
        case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
          implicit val tag = tagged.tag
          (splitIfLongSequence(tagged) filterNot { Some(_) == ps })
      }) ++
      {
        implicit val tag = compositor.tag
        compositor.particles collect { case Compositor(comp) => comp } flatMap {compositorsR(_)}
      }

    val nonps = decl.compositors flatMap {compositorsR(_)}
    if (singleps) nonps
    else ps.toSeq ++ nonps 
  }

  def generateComplexTypeEntity(sym: ClassSymbol, decl: Tagged[XComplexType]): Trippet = {
    logger.debug("generateComplexTypeEntity: emitting %s" format sym.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)
    lazy val allRef: Tagged[AllParam] = Tagged(AllParam(), decl.tag)

    val attributes = decl.flattenedAttributes
    val list =
      (decl.primaryAll map { _ => Seq(allRef) } getOrElse {
        splitParticlesIfLong(decl.particles, decl.tag)}) ++
      (attributes.headOption map { _ => attributeSeqRef }).toSeq
    
    logger.debug("generateComplexTypeEntity: list: %s", list map {getName})

    val paramList: Seq[Param] = Param.fromSeq(list)
    val compositors = compositorsR(decl)
    val compositorCodes = compositors.toList map {generateCompositor}
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.occurrence.isMultiple) &&
          (!paramList.head.attribute) && (!decl.mixed) // && (!longAll)
    val paramsTrees =
      if (hasSequenceParam) paramList.head.varargTree :: Nil
      else paramList map {_.tree}

    val accessors: Seq[Tree] =
      (decl.primaryAll map { generateAllAccessors(_) } getOrElse {
        splitParticles(decl.particles, decl.tag) map { generateLongSeqAccessors(_) } getOrElse {Nil} }) ++
      (attributes.headOption map  { _ => generateAttributeAccessors(attributes, true) } getOrElse {Nil})
    val parents = complexTypeSuperTypes(decl)

    Trippet(
      Trippet(CASECLASSDEF(sym) withParams(paramsTrees) withParents(parents) :=
        (if (accessors.isEmpty) EmptyTree
        else BLOCK(accessors: _*)),
        EmptyTree,
        generateDefaultFormat(sym, decl),
        makeImplicitValue(sym)) ::
      compositorCodes: _*)
  }

  private def generateDefaultFormat(sym: ClassSymbol, decl: Tagged[XComplexType]): Tree = {
    val particles = decl.particles
    val unmixedParserList = particles map { buildParser(_, decl.mixed, decl.mixed) }
    val parserList = if (decl.mixed) buildTextParser +: (unmixedParserList flatMap { Seq(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList: Seq[Tree] = ( 0 to parserList.size - 1) map { i => buildSelector(i) }

    val particleArgs: Seq[Tree] = if (decl.mixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(particles((i - 1) / 2), i)
        else buildArgForOptTextRecord(i) }
      else decl.primaryAll map { all => 
        implicit val tag = all.tag
        all.particles map { buildArgForAll(_) } } getOrElse {
        particles.zipWithIndex map { case (i, x) => buildArg(i, x) }
      }

    val simpleFromXml =
      if (particles.isEmpty && !decl.mixed) true
      else (decl.primaryAll) match {
        case Some(x) => true
        case _ => false
      }

    def makeWritesChildNodes: Option[Tree] = {
      def simpleContentTree(base: QualifiedName): Tree = base match {
        case BuiltInAnyType(_) => SEQ(TextClass APPLY(REF("__obj") DOT "value" DOT "value" DOT "toString"))
        case _ => SEQ(TextClass APPLY(REF("__obj") DOT "value" DOT "toString"))
      }
      def toXMLArgs: List[Tree] = REF("x") :: (REF("x") DOT "namespace").tree :: (REF("x") DOT "key").tree :: REF("__scope") :: FALSE :: Nil
      def childTree: Tree = if (decl.mixed) (REF("__obj") DOT makeParamName(MIXED_PARAM) DOT "toSeq") FLATMAP LAMBDA(PARAM("x")) ==> BLOCK(
          buildToXML(DataRecordAnyClass, toXMLArgs)
        )
      else decl.value.arg1.value match {
        case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => simpleContentTree(x.base)
        case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => simpleContentTree(x.base)
        case _ =>
          if (particles.isEmpty) NIL
          else if (particles.size == 1) PAREN(buildXMLTree(Param(particles(0))))
          else (SeqClass DOT "concat")(Param.fromSeq(particles) map { x => buildXMLTree(x) })
      }

      Some(DEF("writesChildNodes", TYPE_SEQ(NodeClass)) withParams(PARAM("__obj", sym),
        PARAM("__scope", NamespaceBindingClass)) := childTree)
    }

    val makeWritesAttribute = None

    val groups = decl.flattenedGroups filter { case tagged: TaggedGroupRef =>
      implicit val tag = tagged.tag
      resolveNamedGroup(tagged.ref.get).particles.size > 0 }
    val defaultFormatSuperNames: Seq[Type] =
      (ElemNameParserClass TYPE_OF sym) ::
      (groups.toList map { case tagged: TaggedGroupRef =>
        formatterSymbol(userDefinedClassSymbol(tagged)): Type })
    
    val fmt = formatterSymbol(sym)

    if (simpleFromXml)
      TRAITDEF("Default" + fmt.decodedName) withParents(xmlFormatType(sym) :: (CanWriteChildNodesClass TYPE_OF sym) :: Nil) := BLOCK(List(
        Some(VAL("targetNamespace", TYPE_OPTION(StringClass)) := optionUriTree(schema.targetNamespace)),
        Some(DEF("reads", eitherType(StringClass, fmt)) withParams(
          PARAM("seq", NodeSeqClass), PARAM("stack", TYPE_LIST(ElemNameClass))) := REF("seq") MATCH(
          CASE (ID("node") withType(NodeClass)) ==> (REF("Right") APPLY(sym APPLY particleArgs)),
          CASE (WILDCARD) ==> (REF("Left") APPLY LIT("reads failed: seq must be scala.xml.Node"))
        )),
        makeWritesAttribute,
        makeWritesChildNodes
      ).flatten)
    else
      TRAITDEF("Default" + fmt.decodedName) withParents(defaultFormatSuperNames) := BLOCK(List(
        Some(VAL("targetNamespace", TYPE_OPTION(StringClass)) := optionUriTree(schema.targetNamespace)),
        decl.name map { typeName =>
          DEF("typeName", TYPE_OPTION(StringClass)) withFlags(Flags.OVERRIDE) := optionTree(decl.name)
        },
        if (decl.mixed) Some(DEF("isMixed", BooleanClass) withFlags(Flags.OVERRIDE) := TRUE)
        else None,
        Some(DEF("parser", ParserClass TYPE_OF sym) withParams(
            PARAM("node", "scala.xml.Node"), PARAM("stack", TYPE_LIST("scalaxb.ElemName"))) :=
          INFIX_CHAIN("~", parserList) INFIX("^^") APPLY BLOCK(
            CASE(INFIX_CHAIN("~", parserVariableList)) ==> REF(sym) APPLY particleArgs
          )
        ),
        makeWritesAttribute,
        makeWritesChildNodes
      ).flatten)
  }

  private def makeImplicitValue(sym: ClassSymbol): Tree = {
    val fmt = formatterSymbol(sym)
    LAZYVAL(fmt) withFlags(Flags.IMPLICIT) withType(xmlFormatType(sym)) :=
      NEW("Default" + fmt.decodedName)    
  }

  def complexTypeSuperTypes(decl: Tagged[XComplexType]): Seq[Type] = {
    decl.attributeGroups map {buildType}
  }

  def generateSequence(tagged: TaggedParticle[KeyedGroup]): Trippet = {
    logger.debug("generateSequence: %s", tagged)
    
    val name = getName(tagged)
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
    val list = splitParticlesIfLong(tagged.particles, tagged.tag)
    val paramList = Param.fromSeq(list)
    // Snippet(<source>case class { name }({
    //  paramList.map(_.toScalaCode).mkString(", " + NL + indent(1))})</source>)
    Trippet(CASECLASSDEF(name) withParams(paramList map {_.tree}: _*))
  }

  def generateCompositor(decl: TaggedParticle[KeyedGroup]): Trippet = decl.key match {
    case SequenceTag => generateSequence(decl)
    case _ =>
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
      val superString = ""
      val name = getName(decl)
      // Snippet(<source>trait {name}{superString}</source>)
      Trippet(TRAITDEF(name))
  }

  def processSimpleType(decl: Tagged[XSimpleType]): Seq[Trippet] =
    Seq(generateSimpleType(userDefinedClassSymbol(decl), decl))

  def generateSimpleType(sym: ClassSymbol, decl: Tagged[XSimpleType]) = {
    val enumValues = filterEnumeration(decl) map { enum =>
      CASEOBJECTDEF(userDefinedClassSymbol(enum)) withParents(sym) := BLOCK(
        DEF(Any_toString) withFlags(Flags.OVERRIDE) := LIT(enum.value.value)
      )
    }

    Trippet(TRAITDEF(sym).tree :: enumValues.toList, Nil, Nil, Nil)
  }
  
  def processAttributeGroup(tagged: Tagged[XAttributeGroup]): Seq[Trippet] =
    Seq(generateAttributeGroup(buildAttributeGroupTypeSymbol(tagged), tagged))

  def generateAttributeGroup(sym: ClassSymbol, tagged: Tagged[XAttributeGroup]): Trippet = {
    val accessors = generateAttributeAccessors(tagged.flattenedAttributes, false)
    Trippet(TRAITDEF(sym) := BLOCK(accessors: _*))
  }

  def generateAllAccessors(tagged: TaggedParticle[KeyedGroup]): Seq[Tree] = {
    implicit val tag = tagged.tag
    val paramList = Param.fromSeq(tagged.particles)
    paramList map {_.toDataRecordMapAccessor(makeParamName(ALL_PARAM), true)}
  }

  def generateLongSeqAccessors(splits: Seq[TaggedParticle[KeyedGroup]]): Seq[Tree] =
    splits flatMap { sequence =>
      implicit val tag = sequence.tag
      val wrapper = Param.fromSeq(Seq(sequence)).head
      val paramList = Param.fromSeq(sequence.particles)
      paramList map { _.toLongSeqAccessor(wrapper.paramName) }
    }

  def generateAttributeAccessors(attributes: Seq[Tagged[_]], generateImpl: Boolean): Seq[Tree] =
    Param.fromAttributes(attributes) map {_.toDataRecordMapAccessor(makeParamName(ATTRS_PARAM), generateImpl)}

  def packageCode: Seq[Node] =
    (packageNameByURI(schema.targetNamespace, context) map { pkg =>
      <source>package {pkg}</source>
    }).toSeq
}
