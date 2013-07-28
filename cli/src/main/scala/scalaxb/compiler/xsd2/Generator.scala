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
import scalaxb.compiler.{Config, Snippet, Trippet, Log}
import xmlschema._
import Defs._

class Generator(val schema: ReferenceSchema, 
    val context: SchemaContext, val config: Config) extends Params with PackageNamer
    with Namer with Lookup with Splitter with Parsers with Args with XMLOutputs with Symbols {
  import Predef.{any2stringadd => _}
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
        case _ => Vector()
      }): _*)

  def processComplexType(decl: TaggedType[XComplexType]): Seq[Trippet] =
    if (context.baseToSubs.contains(decl)) {
      generateBaseComplexTypeTrait(buildTraitSymbol(decl), decl) +: 
      (if (decl.abstractValue) Vector()
      else generateComplexTypeEntity(buildComplexTypeSymbol(decl), decl) +: Vector())
    }
    else Seq(generateComplexTypeEntity(buildComplexTypeSymbol(decl), decl))

  /** recursively return compositors.
   */
  private def compositorsR(decl: TaggedType[XComplexType]): Seq[TaggedParticle[KeyedGroup]] = {
    val ps = decl.primarySequence
    val singleps = ps map { tagged => Occurrence(tagged).isSingle } getOrElse {false}

    def compositorsR(compositor: TaggedParticle[KeyedGroup]): Seq[TaggedParticle[KeyedGroup]] =
      (compositor match {
        case x: TaggedKeyedGroup if x.value.key == AllTag => Seq(x)
        case tagged: TaggedKeyedGroup if tagged.particles.isEmpty => Vector()
        case x: TaggedKeyedGroup if x.value.key == ChoiceTag => Seq(x)
        case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
          implicit val tag = tagged.tag
          (splitIfLongSequence(tagged) filterNot { Some(_) == ps })
        case _ => Vector()
      }) ++
      {
        implicit val tag = compositor.tag
        compositor.particles collect { case Compositor(comp) => comp } flatMap {compositorsR(_)}
      }

    val nonps = (decl.compositors flatMap {compositorsR(_)}).distinct
    if (singleps) nonps
    else ps.toSeq ++ nonps 
  }

  def generateComplexTypeEntity(sym: ClassSymbol, decl: TaggedType[XComplexType]): Trippet = {
    logger.debug("generateComplexTypeEntity: emitting %s" format sym.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)
    lazy val mixedSeqRef: Tagged[MixedSeqParam] = TaggedMixedSeqParam(MixedSeqParam(), decl.tag)

    val attributes = decl.flattenedAttributes
    val list =
      (if (decl.effectiveMixed) Seq(mixedSeqRef)
      else decl.primaryCompositor map { _ => decl.splitNonEmptyParticles } getOrElse {
        if (decl.hasSimpleContent) Seq(decl.simpleContentRoot)
        else Vector()
      }) ++
      (attributes.headOption map { _ => attributeSeqRef }).toSeq
    val longAll = decl.primaryAll map {_ => true} getOrElse {false}
    
    logger.debug("generateComplexTypeEntity: list: %s", list map {getName})

    val paramList: Seq[Param] = Param.fromSeq(list)
    val compositors = compositorsR(decl)
    val compositorCodes = compositors.toList map { x => generateCompositor(sym.owner.newClass(getName(x)), x) }
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.occurrence.isMultiple) &&
          (!paramList.head.attribute) && (!decl.effectiveMixed) && (!longAll)
    val accessors: Seq[Tree] =
      (decl.primaryCompositor map {
        case x: TaggedKeyedGroup if x.value.key == AllTag => generateAllAccessors(x)
        case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
          splitLongSequence(tagged) map {
            generateLongSeqAccessors(_)
          } getOrElse Vector()
        case _ => Vector()
      } getOrElse Vector()) ++
      (attributes.headOption map  { _ => generateAttributeAccessors(attributes, true) } getOrElse Vector())
    val parents: Seq[Type] = 
      if (context.baseToSubs.contains(decl)) Seq(buildTraitSymbol(decl): Type)
      else complexTypeSuperTypes(decl)

    Trippet(
      Trippet(CASECLASSDEF(sym) withParams(
          if (hasSequenceParam) paramList.head.varargTree +: Vector()
          else paramList map {_.tree}) withParents(parents) :=
        (if (accessors.isEmpty) EmptyTree
        else BLOCK(accessors: _*)),
        EmptyTree,
        generateDefaultFormat(sym, decl, attributes, hasSequenceParam, longAll),
        makeImplicitValue(sym)) ::
      compositorCodes: _*)
  }

  private def DATARECORD(tree: Tree*): Tree = DataRecordModule APPLY(tree: _*)

  private def generateDefaultFormat(sym: ClassSymbol, decl: TaggedType[XComplexType],
      attributes: Vector[TaggedAttr[_]], hasSequenceParam: Boolean, longAll: Boolean): Tree = {
    val particles = decl.splitNonEmptyParticles
    val unmixedParserList = particles map { buildParticleParser(_, decl.effectiveMixed, decl.effectiveMixed, false) }
    val parserList = if (decl.effectiveMixed) buildTextParser +: (unmixedParserList flatMap { Seq(_, buildTextParser) })
      else unmixedParserList
    val parserVariableList: Seq[Tree] = ( 0 to parserList.size - 1) map { i => buildSelector(i) }

    val particleArgs: Seq[Tree] =
      if (decl.effectiveMixed) (0 to parserList.size - 1).toList map { i =>
        if (i % 2 == 1) buildArgForMixed(particles((i - 1) / 2), i, false)
        else buildArgForOptTextRecord(i) }
      else decl.primaryAll map { all => 
        implicit val tag = all.tag
        all.particles map { buildArgForAll(_) } } getOrElse {
        particles.zipWithIndex map { case (i, x) => buildArg(i, x) }
      }
    val simpleFromXml =
      if (particles.isEmpty && !decl.effectiveMixed) true
      else if (decl.hasSimpleContent) true
      else (decl.primaryAll) match {
        case Some(x) => true
        case _ => false
      }

    def makeWritesChildNodes: Option[Tree] = {
      def simpleContentTree(base: TaggedType[_]): Tree = base match {
        case AnyLike(_) =>
          (REF("__obj") DOT "value" DOT "value") MATCH(
            CASE(ID("elem") withType(ElemClass)) ==> (REF("elem") DOT "child"),
            CASE(ID("x")) ==> SEQ(TextClass APPLY(REF("x") DOT "toString"))
          )
        case _ => SEQ(TextClass APPLY(REF("__obj") DOT "value" DOT "toString"))
      }
      def toXMLArgs: List[Tree] = REF("x") :: (REF("x") DOT "namespace").tree :: (REF("x") DOT "key").tree :: REF("__scope") :: FALSE :: Nil
      def childTree(decl: TaggedType[XComplexType]): Tree =
        if (decl.effectiveMixed) (REF("__obj") DOT makeParamName(MIXED_PARAM) DOT "toSeq") FLATMAP LAMBDA(PARAM("x")) ==> BLOCK(
            buildToXML(DataRecordAnyClass, toXMLArgs)
          )
        else if (decl.hasSimpleContent)
          decl.base match {
            case tagged: TaggedComplexType => childTree(tagged)
            case _ => simpleContentTree(decl.base)
          }
        else if (particles.isEmpty) NIL
        else if (particles.size == 1) PAREN(buildXMLTree(Param(particles(0), 0)))
        else (SeqClass DOT "concat")(Param.fromSeq(particles) map { x => buildXMLTree(x) })

      Some(DEF("writesChildNodes", TYPE_SEQ(NodeClass)) withParams(PARAM("__obj", sym),
        PARAM("__scope", NamespaceBindingClass)) := childTree(decl))
    }

    val makeWritesAttribute: Option[Tree] =
      if (attributes.isEmpty) None
      else Some(DEF("writesAttribute", MetaDataClass) withFlags(Flags.OVERRIDE) withParams(
        PARAM("__obj", sym), PARAM("__scope", NamespaceBindingClass)
      ) := (HelperClass DOT "mapToAttributes")(REF("__obj") DOT "attributes", REF("__scope")))

    val groups =
      (decl.flattenedGroups map { ref =>
        resolveNamedGroup(ref.ref.get) 
      } filter { tagged =>
        (tagged.primaryCompositor map { c =>
          c.particles.size
        } getOrElse {0}) > 0
      }).distinct
    logger.debug("generateDefaultFormat: groups: %s", groups map {getName})

    val defaultFormatSuperNames: Seq[Type] =
      (ElemNameParserClass TYPE_OF sym) +:
      (groups map { case tagged: TaggedNamedGroup =>
        formatterSymbol(buildNamedGroupSymbol(tagged)): Type })
    
    val dfmt = defaultFormatterSymbol(sym)
    val attributeTree: Seq[Tree] =
      if (attributes.isEmpty) Vector()
      else {
        val casetree: Seq[CaseDef] = (attributes collect {
          case x@TaggedAttribute(attr: XTopLevelAttribute, _) =>
            val ns = x.tag.namespace map {_.toString} getOrElse ""
            CASE(ID("attr") withType(PrefixedAttributeClass),
              IF(PAREN((REF("elem") DOT "scope" DOT "getURI")(REF("attr") DOT "pre") ANY_== LIT(ns) AND
                 PAREN((REF("attr") DOT "key") ANY_== LIT(attr.name.get))))) ==>
              BLOCK(
                VAL("ns") := (REF("elem") DOT "scope" DOT "getURI")(REF("attr") DOT "pre"),
                TUPLE(LIT("@{" + ns + "}" + attr.name.get), generateAttribute(x))
              )
          case x@TaggedAttribute(attr: XAttribute, _) =>
            CASE(ID("attr") withType(UnprefixedAttributeClass),
              IF((REF("attr") DOT "key") ANY_== LIT(attr.name.get))) ==>
              TUPLE(LIT("@" + attr.name.get), generateAttribute(x))            
        }) ++    
        Seq(
          CASE(ID("attr") withType(PrefixedAttributeClass)) ==>
            BLOCK(
              VAL("ns") := (REF("elem") DOT "scope" DOT "getURI")(REF("attr") DOT "pre"),
              TUPLE(INFIX_CHAIN("+", LIT("@{"), REF("ns"), LIT("}"), REF("attr") DOT "key"),
                DATARECORD(TYPE_OPTION(StringClass) APPLY REF("ns"), SOME(REF("attr") DOT "key"), REF("attr") DOT "value" DOT "text"))
            ),
          CASE(ID("attr")) ==> TUPLE(LIT("@") INFIX("+", REF("attr") DOT "key"),
              DATARECORD(NONE, SOME(REF("attr") DOT "key"), REF("attr") DOT "value" DOT "text")))
        
        Seq(ListMapClass.module APPLYTYPE(StringClass, DataRecordAnyClass) APPLY SEQARG( PAREN( REF("node") MATCH (
          CASE(ID("elem") withType(ElemClass)) ==> ((REF("elem") DOT "attributes" DOT "toList") MAP(BLOCK(
            casetree: _*
          ))),
          CASE(WILDCARD) ==> NIL
        ) // node MATCH
        )))
      }
    val argsTree: Seq[Tree] =
      (Vector() match {
        case _ if decl.effectiveMixed   => Seq((SeqClass DOT "concat")(particleArgs))
        case _ if decl.hasSimpleContent => Seq(buildSimpleContentArg(decl.simpleContentRoot))
        case _ if hasSequenceParam      => Seq(SEQARG(particleArgs.head))
        case _ if longAll               =>
          Seq(ListMapClass.module APPLY SEQARG((LIST(particleArgs) DOT "flatten") APPLYTYPE
            TYPE_TUPLE(StringClass, DataRecordAnyClass)
          ))
        case _ => particleArgs
      }) ++ attributeTree

    if (simpleFromXml)
      TRAITDEF(dfmt.decodedName) withParents(xmlFormatType(sym) :: (CanWriteChildNodesClass TYPE_OF sym) :: Nil) := BLOCK(List(
        Some(IMPORT(ElemNameClass, "_")),
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
      TRAITDEF(dfmt.decodedName) withParents(defaultFormatSuperNames) := BLOCK(List(
        Some(VAL("targetNamespace", TYPE_OPTION(StringClass)) := optionUriTree(schema.targetNamespace)),
        decl.name map { typeName =>
          DEF("typeName", TYPE_OPTION(StringClass)) withFlags(Flags.OVERRIDE) := optionTree(decl.name)
        },
        if (decl.effectiveMixed) Some(DEF("isMixed", BooleanClass) withFlags(Flags.OVERRIDE) := TRUE)
        else None,
        Some(DEF("parser", parserType(sym)) withParams(
            PARAM("node", NodeClass), PARAM("stack", TYPE_LIST("scalaxb.ElemName"))) :=
          REF("phrase") APPLY (INFIX_CHAIN("~", parserList) INFIX("^^") APPLY BLOCK(
            CASE(INFIX_CHAIN("~", parserVariableList)) ==> (sym APPLY argsTree)
          ))
        ),
        makeWritesAttribute,
        makeWritesChildNodes
      ).flatten)
  }

  private def generateAttribute(attr: TaggedAttr[XAttributable]): Tree =
    DATARECORD(
      attr match {
        case TaggedAttribute(attr: XTopLevelAttribute, _) => TYPE_OPTION(StringClass) APPLY REF("ns")
        case _                                            => NONE  
      },
      SOME(REF("attr") DOT "key"), buildArg(
      attr.typeValue match {
        case Some(BuiltInType(tpe)) => tpe
        case Some(SimpleType(tpe))  => tpe
        case _ => TaggedXsString
      }, REF("attr") DOT "value", false))

  private def generateBaseComplexTypeTrait(sym: ClassSymbol, decl: TaggedType[XComplexType]): Trippet = {
    logger.debug("generateBaseComplexTypeTrait: emitting %s" format sym.toString)

    lazy val attributeSeqRef: Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(AttributeSeqParam(), decl.tag)

    val attributes = decl.flattenedAttributes
    val hasAttributes = !attributes.isEmpty
    val list =
      (if (decl.effectiveMixed) Vector()
      else decl.primaryCompositor map { _ => decl.splitNonEmptyParticles } getOrElse {
        if (decl.hasSimpleContent) Seq(decl.simpleContentRoot)
        else Vector()
      }) ++
      (attributes.headOption map { _ => attributeSeqRef }).toSeq

    val paramList: Seq[Param] = Param.fromSeq(list)
    val parents: Seq[Type] = complexTypeSuperTypes(decl)
    val compositors = compositorsR(decl)
    val compositorCodes = compositors.toList map { x => generateCompositor(sym.owner.newClass(getName(x)), x) }

    Trippet(TRAITDEF(sym) withParents(parents) := BLOCK(
        paramList map {_.traitTree}
      ),
      EmptyTree,
      generateBaseComplexTypeFormat(sym, decl),
      makeImplicitValue(sym))
  }

  private def generateBaseComplexTypeFormat(sym: ClassSymbol, decl: TaggedType[XComplexType]): Tree = {
    logger.debug("generateBaseComplexTypeFormat - ", sym)

    val dfmt = defaultFormatterSymbol(sym)

    def makeReadsXML: Tree = {
      def caseTrees: Seq[CaseDef] =
        (decl.descendants filter {_.name.isDefined} map { sub =>
          CASE(TUPLE(optionUriTree(schema.targetNamespace), optionTree(sub.name))) ==>
            RIGHT(buildFromXML(buildComplexTypeSymbol(sub), REF("node"), REF("stack"), None))
        }) ++
        (if (!decl.abstractValue) Seq(CASE(WILDCARD) ==> RIGHT(buildFromXML(buildComplexTypeSymbol(decl), REF("node"), REF("stack"), None)))
        else Seq(CASE(ID("x")) ==> LEFT(LIT("unknown type: ") INFIX("+") APPLY REF("x"))))
      DEF("reads", eitherType(StringClass, sym)) withParams(
        PARAM("seq", NodeSeqClass), PARAM("stack", TYPE_LIST(ElemNameClass))) := 
          REF("seq") MATCH(
            CASE(ID("node") withType(NodeClass)) ==>
              ((HelperClass DOT "instanceType")(REF("node")) MATCH(caseTrees: _*)),
            CASE(WILDCARD) ==> LEFT(LIT("reads failed: seq must be scala.xml.Node"))  
          )
    }

    def makeWritesXML: Tree = {
      def caseTrees: Seq[CaseDef] =
        (decl.descendants map { sub =>
          CASE(ID("x") withType(buildComplexTypeSymbol(sub))) ==>
            buildToXML(buildComplexTypeSymbol(sub), REF("x") :: REF("__namespace") :: REF("__elementLabel") :: REF("__scope") :: TRUE :: Nil)
        }) ++
        (if (!decl.abstractValue) Seq(
          CASE(ID("x") withType(buildComplexTypeSymbol(decl))) ==>
            buildToXML(buildComplexTypeSymbol(decl), REF("x") :: REF("__namespace") :: REF("__elementLabel") :: REF("__scope") :: FALSE :: Nil))
        else Seq(
          CASE(WILDCARD) ==> (REF("sys") DOT "error")(LIT("unknown type: ") INFIX("+") APPLY REF("__obj"))))
      DEF("writes", NodeSeqClass) withParams(PARAM("__obj", sym),
        PARAM("__namespace", optionType(StringClass)),
        PARAM("__elementLabel", optionType(StringClass)),
        PARAM("__scope", NamespaceBindingClass),
        PARAM("__typeAttribute", BooleanClass)) := REF("__obj") MATCH(caseTrees: _*)
    }
    
    TRAITDEF(dfmt.decodedName) withParents(XMLFormatClass TYPE_OF sym) := BLOCK(
      makeReadsXML,
      makeWritesXML
    )    
  }

  private def makeImplicitValue(sym: ClassSymbol): Tree = {
    val fmt = formatterSymbol(sym)
    val dfmt = defaultFormatterSymbol(sym)
    LAZYVAL(fmt) withFlags(Flags.IMPLICIT) withType(xmlFormatType(sym)) :=
      NEW(ANONDEF(dfmt.decodedName) := BLOCK())   
  }

  private def complexTypeSuperTypes(decl: TaggedType[XComplexType]): Seq[Type] = {
    def choices: List[TaggedKeyedGroup] =
      (for {
        sch <- context.schemas
        choice <- WrappedSchema.choiceList(sch.unbound)
        p <- choice.particles
      } yield p match {
        case x: TaggedLocalElement =>
          val elem = x.ref match {
            case Some(Element(elem)) => elem
            case _ => x
          }
          elem.typeStructure match {
            case x: TaggedComplexType if x == decl => Some(choice)
            case _ => None
          }
        case _ => None
      }).flatten.toList.distinct

    (decl.base match {
      case base: TaggedComplexType => Seq(buildTraitSymbol(base): Type)
      case _ => Vector() 
    }) ++
    (choices map {buildBaseType}) ++
    (decl.attributeGroups map {buildType})
  }

  private def compositorSuperTypes(tagged: TaggedParticle[KeyedGroup]): Seq[Type] = {
    def choices: List[TaggedKeyedGroup] =
      (for {
        sch <- context.schemas
        choice <- WrappedSchema.choiceList(sch.unbound)
        p <- choice.particles
      } yield p match {
        case x: TaggedKeyedGroup if x == tagged => Some(choice)
        case _ => None
      }).flatten.toList.distinct

    choices map {buildBaseType}
  }

  def generateSequence(sym: ClassSymbol, tagged: TaggedParticle[KeyedGroup]): Trippet = {
    logger.debug("generateSequence: %s", tagged)
    
    val parents: Seq[Type] = compositorSuperTypes(tagged)
    val list = splitLongSequence(tagged) getOrElse {tagged.particles}
    val paramList = Param.fromSeq(list)
    val hasSequenceParam = (paramList.size == 1) && (paramList.head.occurrence.isMultiple)    
    Trippet(CASECLASSDEF(sym) withParams(
      (if (hasSequenceParam) paramList.head.varargTree :: Nil
      else paramList map {_.tree}): _*) withParents(parents),
      EmptyTree,
      generateSequenceFormat(sym, tagged),
      makeImplicitValue(sym))
  }

  private[this] def generateSequenceFormat(sym: ClassSymbol, tagged: TaggedParticle[KeyedGroup]): Tree = {
    logger.debug("generateSequenceFormat")
    val list = splitLongSequence(tagged) getOrElse {tagged.particles}
    val paramList = Param.fromSeq(list)
    val dfmt = defaultFormatterSymbol(sym)

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
    
    TRAITDEF(dfmt.decodedName) withParents(XMLFormatClass TYPE_OF sym) := BLOCK(List(
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

  def processSimpleType(decl: TaggedType[XSimpleType]): Seq[Trippet] =
    Seq(generateSimpleType(userDefinedClassSymbol(decl), decl))

  private def generateSimpleType(sym: ClassSymbol, decl: TaggedType[XSimpleType]) = {
    val enums = filterEnumeration(decl)
    val enumValues = enums map { enum =>
      CASEOBJECTDEF(userDefinedClassSymbol(enum)) withParents(sym) := BLOCK(
        DEF(Any_toString) withFlags(Flags.OVERRIDE) := LIT(decl.enumValue(enum).toString)
      )
    }
    val valueTree: Tree =
      if (decl.qnameBased) PAREN(BLOCK(
          VAL(TUPLE(ID("ns"), ID("localPart"))) := (HelperClass DOT "splitQName") APPLY(REF("value"), REF("scope")),
          NEW(QNameClass, REF("ns") DOT "orNull", REF("localPart")) TOSTRING
        ))
      else REF("value")
    val companionTree: Tree = 
      if (enums.isEmpty) OBJECTDEF(sym) := BLOCK(
        DEF("fromString", sym) withParams(PARAM("value", StringClass), PARAM("scope", NamespaceBindingClass)) := REF(sym) APPLY()
        )
      else OBJECTDEF(sym) := BLOCK(
        DEF("fromString", sym) withParams(PARAM("value", StringClass), PARAM("scope", NamespaceBindingClass)) := valueTree MATCH(
          enums map { enum => CASE (LIT(decl.enumValue(enum).toString)) ==> REF(userDefinedClassSymbol(enum)) }
        ))
    Trippet(TRAITDEF(sym).tree :: companionTree ::
      enumValues.toList, Nil,
      generateSimpleTypeFormat(sym, decl) :: Nil,
      makeImplicitValue(sym) :: Nil)
  }

  private def generateSimpleTypeFormat(sym: ClassSymbol, decl: TaggedType[XSimpleType]): Tree = {
    val dfmt = defaultFormatterSymbol(sym)
    
    TRAITDEF(dfmt.decodedName) withParents(XMLFormatClass TYPE_OF sym) := BLOCK(List(
      DEF("reads", eitherType(StringClass, sym)) withParams(
          PARAM("seq", NodeSeqClass), PARAM("stack", TYPE_LIST(ElemNameClass))) := REF("seq") MATCH(
          CASE(ID("elem") withType(ElemClass)) ==> RIGHT((sym DOT "fromString")(REF("elem") DOT "text", REF("elem") DOT "scope")),
          CASE(WILDCARD) ==> RIGHT((sym DOT "fromString")(REF("seq") DOT "text", REF(TopScopeModule)))
        ),
      DEF("writes", NodeSeqClass) withParams(PARAM("__obj", sym),
        PARAM("__namespace", optionType(StringClass)),
        PARAM("__elementLabel", optionType(StringClass)),
        PARAM("__scope", NamespaceBindingClass),
        PARAM("__typeAttribute", BooleanClass)) :=
        ElemClass APPLY((HelperClass DOT "getPrefix").APPLY(REF("__namespace"), REF("__scope")) DOT "orNull",
          REF("__elementLabel") INFIX("getOrElse") APPLY(BLOCK( (REF("sys") DOT "error")(LIT("missing element label.")))),
          REF(NullModule),
          REF("__scope"),
          TextClass APPLY(REF("__obj") DOT "toString"))
    ))    
  }

  def processNamedGroup(tagged: Tagged[XNamedGroup]): Seq[Trippet] = {
    val sym = buildNamedGroupSymbol(tagged)
    val compositors = tagged.compositors
    val primary = tagged.primaryCompositor
    
    if (primary map {_.particles.isEmpty} getOrElse {false}) Vector()
    else {
      val compositorCodes = compositors flatMap { x =>
        Vector(generateCompositor(userDefinedClassSymbol(x), x)) ++
        (splitLongSequence(x) map { _ map { x =>
          generateCompositor(userDefinedClassSymbol(x), x)
        }} getOrElse Vector())
      }
      Seq(Trippet(Trippet(EmptyTree, EmptyTree,
        generateNamedGroupFormat(sym, tagged), EmptyTree) +: compositorCodes: _*))
    }
  }
  
  def generateNamedGroupFormat(sym: ClassSymbol, tagged: Tagged[XNamedGroup]): Tree =
    tagged.primaryCompositor map {
      case compositor if compositor.particles.isEmpty => EmptyTree
      case compositor =>
        val fmt = formatterSymbol(sym)
        val param = Param(compositor, 0)
        val o = Occurrence(compositor).toSingle
        val parser = buildKeyedGroupParser(compositor, o, false, false, false)
        val (wrapperParam, wrapperParser) = compositor match {
          case x: TaggedKeyedGroup if x.key == ChoiceTag => (param, parser)
          case _ =>
            param.copy(typeSymbol = TaggedDataRecordSymbol(DataRecordSymbol(param.typeSymbol))) ->
            buildKeyedGroupParser(compositor, o, false, true, false)
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
            buildKeyedGroupParser(compositor, Occurrence.SingleNotNillable(), true, true, false)
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
