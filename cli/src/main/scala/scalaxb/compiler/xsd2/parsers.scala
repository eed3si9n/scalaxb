package scalaxb.compiler.xsd2

trait Parsers { self: Namer with Lookup with Args with Params with Symbols with Splitter =>
  import Predef.{any2stringadd => _, _}
  import scalaxb.compiler.Log
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsAnySimpleType, XsTypeSymbol, XsInt}
  import Defs._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
  import xmlschema.{XExplicitGroup}

  private val logger = Log.forName("xsd2.Parsers")

  def buildTextParser: Tree = REF("optTextRecord")

  // called by makeCaseClassWithType and buildSeqParser
  def buildParticleParser(tagged: TaggedParticle[_], mixed: Boolean, wrapInDataRecord: Boolean): Tree =
    buildParticleParser(tagged, Occurrence(tagged), mixed, wrapInDataRecord)

  private[this] def buildParticleParser(particle: TaggedParticle[_], occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): Tree = particle match {
    case tagged: TaggedLocalElement  => buildElemParser(tagged, occurrence, mixed, wrapInDataRecord, false)
    case tagged: TaggedGroupRef      => buildGroupRefParser(tagged, occurrence, mixed, wrapInDataRecord)
    case tagged: TaggedKeyedGroup    => buildKeyedGroupParser(tagged, occurrence, mixed, wrapInDataRecord)
    case tagged: TaggedWildCard      =>
      val namespaceConstraint = if (tagged.value.namespace == "") Nil
                                else tagged.value.namespace.split(' ').toList
      buildWildCardParser(namespaceConstraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
  }
  
  private[this] def buildParserTree(tagged: TaggedLocalElement, occurrence: Occurrence): Tree = {
    val elem = tagged.resolve
    buildParserTree(ElemNameClass APPLY (elementNamespaceTree(elem), LIT(elem.name.get)), occurrence)
  }

  private[this] def buildParserTree(base: Tree, occurrence: Occurrence): Tree =
    if (occurrence.isMultiple) REF("rep") APPLY base
    else if (occurrence.isOptional) REF("opt") APPLY base
    else PAREN(base)

  private[this] def buildElemParser(tagged: TaggedLocalElement, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean,
                      ignoreSubGroup: Boolean): Tree = {
    import Occurrence._

    def buildConverter(typeSymbol: TaggedType[_], occurrence: Occurrence): Tree = {
      val record = DataRecordClass APPLY(REF("x") DOT "namespace",
        SOME(REF("x") DOT "name"),
        buildTypeSymbolArg(REF("x"), typeSymbol))

      val nillableRecord = DataRecordClass APPLY(REF("x") DOT "namespace",
        SOME(REF("x") DOT "name"),
        REF("x") DOT "nilOption" MAP buildTypeSymbolArg(WILDCARD, typeSymbol))

      (occurrence) match {
        case UnboundedNillable(_)    => PAREN((WILDCARD DOT "toSeq") MAP LAMBDA(PARAM("x")) ==> BLOCK(nillableRecord))
        case UnboundedNotNillable(_) => PAREN((WILDCARD DOT "toSeq") MAP LAMBDA(PARAM("x")) ==> BLOCK(record))
        case OptionalNillable(_)     => PAREN(WILDCARD MAP LAMBDA(PARAM("x")) ==> BLOCK(nillableRecord))
        case OptionalNotNillable(_)  => PAREN(WILDCARD MAP LAMBDA(PARAM("x")) ==> BLOCK(record))
        case SingleNillable(_)       => PAREN(LAMBDA(PARAM("x")) ==> BLOCK(nillableRecord))
        case SingleNotNillable(_)    => PAREN(LAMBDA(PARAM("x")) ==> BLOCK(record))
      }
    }

    def addConverter(p: Tree): Tree =
      if (wrapInDataRecord) PAREN(p INFIX("^^") APPLY buildConverter(tagged.typeStructure, occurrence))
      else p

    // if (isSubstitionGroup(elem) && !ignoreSubGroup) addConverter(buildSubstitionGroupParser(elem, occurrence, mixed))
    // else tagged.typeStructure match { ...
    tagged.typeStructure match {
      case x: TaggedSymbol =>
        x.value match {
          case XsAnySimpleType | XsAnyType => buildWildCardParser(Nil, occurrence, mixed, wrapInDataRecord, true)
          case symbol: BuiltInSimpleTypeSymbol => addConverter(buildParserTree(tagged, occurrence))
        }

      case TaggedSimpleType(_, _) | TaggedComplexType(_, _) =>  addConverter(buildParserTree(tagged, occurrence))

//      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
//        if (compositorWrapper.contains(decl)) {
//          val compositor = compositorWrapper(decl)
//          val o = buildOccurrence(compositor)
//          buildCompositorParser(compositor, occurrence.copy(nillable = o.nillable), mixed, wrapInDataRecord)
//        }
//        else addConverter(buildParserString(elem, occurrence))
//      case AnyType(XsWildcard(constraint)) => buildAnyParser(constraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
//      case AnyType(symbol) => buildAnyParser(Nil, occurrence, mixed, wrapInDataRecord, true)
//      case XsLongAll => ""
//
//      case symbol: ReferenceTypeSymbol =>
//        if (symbol.decl == null)
//          error("Parsers#buildParser: " + elem.toString +
//            " Invalid type " + symbol.getClass.toString + ": " +
//            symbol.toString + " with null decl")
//        else
//          error("Parsers#buildParser: " + elem.toString +
//            " Invalid type " + symbol.getClass.toString + ": " +
//            symbol.toString + " with " + symbol.decl.toString)
//      case _ => error("Parsers#buildParser: " + elem.toString +
//        " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
    }
  }

  private[this] def buildStackTree: Tree = (ElemNameClass APPLY(REF("node"))) LIST_:: REF("stack")

  private[this] def buildWildCardParser(namespaceConstraint: List[String], occurrence: Occurrence, mixed: Boolean,
                          wrapInDataRecord: Boolean, laxAny: Boolean): Tree = {
    def converter: Tree =
      if (occurrence.nillable) buildFromXML(nillableWildCardType, WILDCARD,
        buildStackTree, None)
      else buildFromXML(wildCardType, WILDCARD, buildStackTree, None)

    def parser: Tree =
      REF("any") APPLY(
        if (laxAny) LAMBDA(PARAM(WILDCARD)) ==> TRUE
        else namespaceConstraint match {
          case Nil =>               LAMBDA(PARAM(WILDCARD)) ==> TRUE
          case "##any" :: Nil =>    LAMBDA(PARAM(WILDCARD)) ==> TRUE
          case "##other" :: Nil =>  (WILDCARD DOT "namespace") ANY_!= optionUriTree(schema.targetNamespace)
          case _ =>
            val list = LIST(namespaceConstraint map {
              case "##targetNamespace" => optionUriTree(schema.targetNamespace)
              case "##local" => NONE
              case x => SOME(LIT(x))
            })

            LAMBDA(PARAM("x")) ==> list INFIX("contains") APPLY(REF("x") DOT "namespace")
        }
      )

    def mixedParser: Tree =
      PAREN(
        PAREN(parser INFIX("^^") APPLY PAREN(converter)) INFIX("~") APPLY buildTextParser
      ) INFIX("^^") APPLY BLOCK (
        CASE(ID("p1") INFIX("~") UNAPPLY ID("p2")) ==> (SeqClass DOT "concat")(SEQ(REF("p1")), REF("p2") DOT "toList")
      )

    def wrappedParser: Tree = PAREN(parser INFIX("^^") APPLY PAREN(converter))

    buildParserTree(if (mixed) mixedParser
      else if (wrapInDataRecord) wrappedParser
      else parser,
      occurrence)
  }

  private[this] def buildGroupRefParser(tagged: TaggedGroupRef, occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): Tree = {
    val group = resolveNamedGroup(tagged)
    def mixedParser: Tree = 
      REF("parsemixed" + getName(group)) APPLY(REF("node"), buildStackTree)
    def parser: Tree =
      if (wrapInDataRecord) REF("parse" + getName(group)) APPLY(REF("node"), buildStackTree, TRUE)
      else REF("parse" + getName(group)) APPLY(REF("node"), buildStackTree)
      
    buildParserTree(if (mixed) mixedParser
      else parser,
      occurrence) 
  }

  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildKeyedGroupParser(tagged: TaggedParticle[KeyedGroup], occurrence: Occurrence, 
      mixed: Boolean, wrapInDataRecord: Boolean): Tree = tagged.value.key match {
    case SequenceTag => 
      // if (containsSingleChoice(seq)) buildChoiceParser(singleChoice(seq), occurrence, mixed)
      buildSeqParser(tagged, occurrence, mixed, wrapInDataRecord)
    case ChoiceTag   =>
      buildChoiceParser(tagged, occurrence, mixed)
    case AllTag      =>
      EmptyTree // this parser will not be used
  }

  // for unmixed wrapped in data record, this should generate Seq(DataRecord(None, None, Foo("1", "2")))
  // for mixed, this should generate
  // Seq(DataRecord(Some("ipo") Some("a"), "1"), DataRecord(None, None, "foo"), DataRecord(Some("ipo") Some("b"), "2"))
  private[this] def buildSeqParser(tagged: TaggedParticle[KeyedGroup],
      occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean): Tree = {
    val ps = tagged.particles
    val particles = if (mixed) ps
      else splitLongSequence(tagged) getOrElse {ps}
    val parserList: Seq[Tree] = if (mixed) (0 to ps.size * 2 - 1).toList map { i =>
        if (ps.size == 0) buildTextParser
        else if (i % 2 == 0) buildParticleParser(ps(i / 2), mixed, mixed)
        else buildTextParser
      }
      else particles map { buildParticleParser(_, mixed, mixed) }
    
    def buildSeqConverter(seq: TaggedParticle[KeyedGroup], mixed: Boolean,
        wrapInDataRecord: Boolean): Tree = {
      val sym = buildKeyedGroupTypeSymbol(seq)
      val parserVariableList = if (mixed) (0 to particles.size * 2 - 1) map { buildSelector }
        else (0 to particles.size - 1) map { buildSelector }
      val argList = if (mixed) (0 to particles.size * 2 - 1).toList map { i =>
          if (i % 2 == 0) buildArgForMixed(particles(i / 2), i)
          else buildArgForOptTextRecord(i) }
        else (0 to particles.size - 1).toList map { i => buildArg(particles(i), i) }
      val paramList = if (mixed) Nil
        else Param.fromSeq(particles)
      val hasSequenceParam = (paramList.size == 1) &&
        (paramList.head.occurrence.isMultiple) &&
        (!mixed)
      val simpleCase = if (hasSequenceParam) sym APPLY (SEQARG(argList.head))
                       else sym APPLY(argList)
      val pat = if (particles.isEmpty) WILDCARD
                else INFIX_CHAIN("~", parserVariableList)
      val rhs = if (mixed) SeqClass DOT "concat" APPLY(argList)
                else if (wrapInDataRecord) DataRecordClass APPLY simpleCase
                else simpleCase
      BLOCK(
        CASE(pat) ==> rhs
      )
    }
    
    val base = PAREN(INFIX_CHAIN("~", parserList)) INFIX("^^") APPLY(
      buildSeqConverter(tagged, mixed, wrapInDataRecord))
    val retval = buildParserTree(base, occurrence)
    logger.debug("buildSeqParser:  " + tagged + NL + retval)
    retval
  }

  // one or more particles may be emptiable within the choices.
  // in such case, treat the whole choice to be minOccurs = 0,
  // but make sure all particles has at least minOccurs = 1.
  // additionally, treat all particles as maxOccurs = 1 and make the whole
  // choice repeatable in case any one particle is repeatable.
  // this may violate the schema, but it is a compromise as long as plurals are
  // treated as Seq[DataRecord].
  private[this] def buildChoiceParser(tagged: TaggedParticle[KeyedGroup], occurrence: Occurrence, mixed: Boolean): Tree = {
    val ps = tagged.particles
    assert(ps.size > 0, "choice has no particles: " + tagged)

    val containsStructure = if (mixed) true
      else ps exists(_ match {
        case elem: TaggedLocalElement => false
        case _ => true
        })
    val singleOccurrence = occurrence.copy(minOccurs = 1, maxOccurs = 1) 
    val parserList = ps filter { !isEmptyCompositor(_) } map {
      case elem: TaggedLocalElement =>
        // SequenceDecl(List(elem), 1, 1, 0)
        if (mixed && containsStructure) buildParticleParser(
          Tagged(KeyedGroup(
            key = SequenceTag,
            particles = Vector(elem),
            minOccurs = 1,
            maxOccurs = "1"), tagged.tag),
          singleOccurrence, mixed, true)
        else buildParticleParser(elem, singleOccurrence, mixed, true)
      case particle => buildParticleParser(particle, singleOccurrence, mixed, true)
    }
    val choiceOperator = if (containsStructure) "|||" 
                         else "|"
    val nonany = if (parserList.size > 0) INFIX_CHAIN(choiceOperator, parserList)
                 else EmptyTree
    val anyList = ps collect {
      case any: TaggedWildCard => buildParticleParser(any, singleOccurrence, mixed, true)
    }
    val base: Tree =
      if (anyList.size > 0)
        if (nonany == EmptyTree) anyList(0)
        else PAREN(nonany) INFIX("|") APPLY anyList(0)
      else nonany
    val retval = buildParserTree(base, occurrence)
    logger.debug("buildChoiceParser:  " + tagged + NL + retval)
    retval
  }
}
