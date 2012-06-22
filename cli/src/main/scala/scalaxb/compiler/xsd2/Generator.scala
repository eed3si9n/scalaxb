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
        case x: TaggedNamedGroup => processNamedGroup(x) map {_.toSnippet}
        case x@TaggedAttributeGroup(group: XNamedAttributeGroup, _) => processAttributeGroup(x) map {_.toSnippet}
        case _ => Nil
      }): _*)

  def processComplexType(decl: Tagged[XComplexType]): Seq[Trippet] =
    Seq(generateComplexTypeEntity(buildComplexTypeSymbol(decl), decl))

  /** recursively return compositors.
   */
  private def compositorsR(decl: Tagged[XComplexType]): Seq[TaggedParticle[KeyedGroup]] = {
    val ps = decl.primarySequence
    val singleps = ps map { tagged => Occurrence(tagged.value).isSingle } getOrElse {false}

    def compositorsR(compositor: TaggedParticle[KeyedGroup]): Seq[TaggedParticle[KeyedGroup]] =
      (compositor match {
        case x: TaggedKeyedGroup if x.value.key == ChoiceTag => Seq(x)
        case x: TaggedKeyedGroup if x.value.key == AllTag => Seq(x)
        case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
          implicit val tag = tagged.tag
          (splitIfLongSequence(tagged) filterNot { Some(_) == ps })
        case _ => Nil
      }) ++
      {
        implicit val tag = compositor.tag
        compositor.particles collect { case Compositor(comp) => comp } flatMap {compositorsR(_)}
      }

    val nonps = (decl.compositors flatMap {compositorsR(_)}).distinct
    if (singleps) nonps
    else ps.toSeq ++ nonps 
  }

  def generateComplexTypeEntity(sym: ClassSymbol, decl: Tagged[XComplexType]): Trippet = {
    logger.debug("generateComplexTypeEntity: emitting %s" format sym.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)

    val attributes = decl.flattenedAttributes
    val hasAttributes = !attributes.isEmpty
    val list =
      decl.splitParticles ++
      (attributes.headOption map { _ => attributeSeqRef }).toSeq
    val longAll = decl.primaryAll map {_ => true} getOrElse {false}
    
    logger.debug("generateComplexTypeEntity: list: %s", list map {getName})

    val paramList: Seq[Param] = Param.fromSeq(list)
    val compositors = compositorsR(decl)
    val compositorCodes = compositors.toList map { x => generateCompositor(sym.owner.newClass(getName(x)), x) }
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.occurrence.isMultiple) &&
          (!paramList.head.attribute) && (!decl.mixed) && (!longAll)
    val accessors: Seq[Tree] =
      (decl.primaryCompositor map {
        case x: TaggedKeyedGroup if x.value.key == AllTag => generateAllAccessors(x)
        case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
          splitLongSequence(tagged) map {
            generateLongSeqAccessors(_)
          } getOrElse {Nil}
        case _ => Nil
      } getOrElse {Nil}) ++
      (attributes.headOption map  { _ => generateAttributeAccessors(attributes, true) } getOrElse {Nil})
    val parents = complexTypeSuperTypes(decl)

    Trippet(
      Trippet(CASECLASSDEF(sym) withParams(
          if (hasSequenceParam) paramList.head.varargTree :: Nil
          else paramList map {_.tree}) withParents(parents) :=
        (if (accessors.isEmpty) EmptyTree
        else BLOCK(accessors: _*)),
        EmptyTree,
        generateDefaultFormat(sym, decl, hasAttributes, hasSequenceParam, longAll),
        makeImplicitValue(sym)) ::
      compositorCodes: _*)
  }

  private def generateDefaultFormat(sym: ClassSymbol, decl: Tagged[XComplexType],
      hasAttributes: Boolean, hasSequenceParam: Boolean, longAll: Boolean): Tree = {
    val particles = decl.splitParticles
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

    val makeWritesAttribute: Option[Tree] =
      if (!hasAttributes) None
      else Some(DEF("writesAttribute", MetaDataClass) withFlags(Flags.OVERRIDE) withParams(
        PARAM("__obj", sym), PARAM("__scope", NamespaceBindingClass)
      ) := (HelperClass DOT "mapToAttributes")(REF("__obj") DOT "attributes", REF("__scope")))

    val groups = decl.flattenedGroups map { ref =>
      resolveNamedGroup(ref.ref.get) 
    } filter { tagged =>
      implicit val tag = tagged.tag
      tagged.particles.size > 0
    }
    val defaultFormatSuperNames: Seq[Type] =
      (ElemNameParserClass TYPE_OF sym) ::
      (groups.toList map { case tagged: TaggedNamedGroup =>
        formatterSymbol(buildNamedGroupSymbol(tagged)): Type })
    
    val fmt = formatterSymbol(sym)
    val argsTree: Seq[Tree] =
      (Nil match {
        case _ if hasSequenceParam =>
          Seq(SEQARG(particleArgs.head))
        case _ if longAll =>
          Seq(ListMapClass.module APPLY SEQARG((LIST(particleArgs) DOT "flatten") APPLYTYPE
            TYPE_TUPLE(StringClass, DataRecordAnyClass)
          ))
        case _ => particleArgs
      }) ++ 
      (if (hasAttributes) Seq((HelperClass DOT "attributesToMap")(REF("node")))
      else Nil)

    if (simpleFromXml)
      TRAITDEF("Default" + fmt.decodedName) withParents(xmlFormatType(sym) :: (CanWriteChildNodesClass TYPE_OF sym) :: Nil) := BLOCK(List(
        Some(VAL("targetNamespace", TYPE_OPTION(StringClass)) := optionUriTree(schema.targetNamespace)),
        Some(DEF("reads", eitherType(StringClass, sym)) withParams(
          PARAM("seq", NodeSeqClass), PARAM("stack", TYPE_LIST(ElemNameClass))) := REF("seq") MATCH(
          CASE (ID("node") withType(NodeClass)) ==> (REF("Right") APPLY(sym APPLY argsTree)),
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
        Some(DEF("parser", parserType(sym)) withParams(
            PARAM("node", "scala.xml.Node"), PARAM("stack", TYPE_LIST("scalaxb.ElemName"))) :=
          INFIX_CHAIN("~", parserList) INFIX("^^") APPLY BLOCK(
            CASE(INFIX_CHAIN("~", parserVariableList)) ==> (sym APPLY argsTree)
          )
        ),
        makeWritesAttribute,
        makeWritesChildNodes
      ).flatten)
  }

  private def makeImplicitValue(sym: ClassSymbol): Tree = {
    val fmt = formatterSymbol(sym)
    LAZYVAL(fmt) withFlags(Flags.IMPLICIT) withType(xmlFormatType(sym)) :=
      NEW(ANONDEF("Default" + fmt.decodedName) := BLOCK())   
  }

  def complexTypeSuperTypes(decl: Tagged[XComplexType]): Seq[Type] = {
    def choices: List[TaggedKeyedGroup] =
      (for {
        sch <- context.schemas
        choice <- WrappedSchema.choiceList(sch.unbound)
        p <- choice.particles
      } yield p match {
        case x: TaggedLocalElement =>
          (x.ref, x.typeValue) match {
            case (Some(Element(elem)), _) =>
              elem.typeValue match {
                case Some(ComplexType(t)) if t == decl => Some(choice)
                case _ => None
              }  
            case (_, Some(ComplexType(t))) if t == decl => Some(choice)
            case _ => None
          }
        case _ => None
      }).flatten.toList.distinct

    (choices map {buildBaseType}) ++
    (decl.attributeGroups map {buildType})
  }

  def generateSequence(sym: ClassSymbol, tagged: TaggedParticle[KeyedGroup]): Trippet = {
    logger.debug("generateSequence: %s", tagged)
    
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
    val list = splitLongSequence(tagged) getOrElse {tagged.particles}
    val paramList = Param.fromSeq(list)
    Trippet(CASECLASSDEF(sym) withParams(paramList map {_.tree}: _*),
      EmptyTree,
      generateSequenceFormat(sym, tagged),
      makeImplicitValue(sym))
  }

  private[this] def generateSequenceFormat(sym: ClassSymbol, tagged: TaggedParticle[KeyedGroup]): Tree = {
    logger.debug("generateSequenceFormat")
    val list = splitLongSequence(tagged) getOrElse {tagged.particles}
    val paramList = Param.fromSeq(list)
    val fmt = formatterSymbol(sym)

    def makeWritesXML: Option[Tree] = {
      def simpleContentTree(base: QualifiedName): Tree = base match {
        case BuiltInAnyType(_) => SEQ(TextClass APPLY(REF("__obj") DOT "value" DOT "value" DOT "toString"))
        case _ => SEQ(TextClass APPLY(REF("__obj") DOT "value" DOT "toString"))
      }
      def toXMLArgs: List[Tree] = REF("x") :: (REF("x") DOT "namespace").tree :: (REF("x") DOT "key").tree :: REF("__scope") :: FALSE :: Nil
 
      def childTree: Tree = if (paramList.isEmpty) NIL
        else if (paramList.size == 1) buildXMLTree(paramList(0))
        else (SeqClass DOT "concat")(paramList map { x => buildXMLTree(x) })

      Some(DEF("writes", NodeSeqClass) withParams(PARAM("__obj", sym),
        PARAM("__namespace", optionType(StringClass)),
        PARAM("__elementLabel", optionType(StringClass)),
        PARAM("__scope", NamespaceBindingClass),
        PARAM("__typeAttribute", BooleanClass)) := childTree)
    }
    
    TRAITDEF("Default" + fmt.decodedName) withParents(XMLFormatClass TYPE_OF sym) := BLOCK(List(
      Some(DEF("reads", eitherType(StringClass, sym)) withParams(
          PARAM("seq", NodeSeqClass), PARAM("stack", TYPE_LIST(ElemNameClass))) := LEFT(LIT("Don't call me!"))),
      makeWritesXML
    ).flatten)
  }

  def generateCompositor(sym: ClassSymbol, decl: TaggedParticle[KeyedGroup]): Trippet = decl.key match {
    case SequenceTag => generateSequence(sym, decl)
    case _ =>
//      val superNames: List[String] = buildOptions(compositor)
//      val superString = if (superNames.isEmpty) ""
//        else " extends " + superNames.mkString(" with ")
      val superString = ""
      // Snippet(<source>trait {name}{superString}</source>)
      Trippet(TRAITDEF(sym))
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

  def processNamedGroup(tagged: Tagged[XNamedGroup]): Seq[Trippet] = {
    val compositors = tagged.compositors
    val compositorCodes = compositors.toList map { x => generateCompositor(userDefinedClassSymbol(x), x) }
    Seq(Trippet(Trippet(EmptyTree, EmptyTree,
      generateNamedGroupFormat(buildNamedGroupSymbol(tagged), tagged), EmptyTree) :: compositorCodes: _*))
  }
  
  def generateNamedGroupFormat(sym: ClassSymbol, tagged: Tagged[XNamedGroup]): Tree =
    tagged.primaryCompositor map { compositor =>
      val fmt = formatterSymbol(sym)
      val param = Param(compositor)
      val parser = buildKeyedGroupParser(compositor, Occurrence.SingleNotNillable(), false, false)
      val (wrapperParam, wrapperParser) = compositor match {
        case x: TaggedKeyedGroup if x.key == ChoiceTag => (param, parser)
        case _ =>
          param.copy(typeSymbol = TaggedDataRecordSymbol(DataRecordSymbol(param.typeSymbol))) ->
          buildKeyedGroupParser(compositor, Occurrence.SingleNotNillable(), false, true)
      }
      val mixedParam = param.copy(typeSymbol = TaggedDataRecordSymbol(DataRecordSymbol(TaggedXsAnyType)))
      val subgroups = compositor.particles collect { case ref: TaggedGroupRef => resolveNamedGroup(ref) }
      val parents: Seq[ClassSymbol] =
        if (subgroups.isEmpty) List(AnyElemNameParserClass)
        else subgroups map { g => formatterSymbol(buildNamedGroupSymbol(g)) }

      TRAITDEF(fmt) withParents(parents) := BLOCK(List(
        DEF("parse" + sym.decodedName, parserType(param.baseType)) withParams(
          PARAM("node", NodeClass),
          PARAM("stack", TYPE_LIST(ElemNameClass))) := parser,
        DEF("parse" + sym.decodedName, parserType(wrapperParam.baseType)) withParams(
          PARAM("node", NodeClass),
          PARAM("stack", TYPE_LIST(ElemNameClass)),
          PARAM("wrap", BooleanClass)) := wrapperParser,
        DEF("parsemixed" + sym.decodedName, parserType(seqType(mixedParam.baseType))) withParams(
          PARAM("node", NodeClass),
          PARAM("stack", TYPE_LIST(ElemNameClass))) :=
          buildKeyedGroupParser(compositor, Occurrence.SingleNotNillable(), true, true)
      ))
    } getOrElse {EmptyTree}

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
