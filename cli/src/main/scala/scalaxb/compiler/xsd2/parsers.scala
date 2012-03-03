package scalaxb.compiler.xsd2

trait Parsers { self: Namer with Lookup with Args with Params with Symbols =>
  import Predef.{any2stringadd => _, _}
  import com.weiglewilczek.slf4s.Logger
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsAnySimpleType, XsTypeSymbol, XsInt}
  import Defs._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private lazy val logger: Logger = Logger("xsd2.Parsers")

  def buildTextParser: Tree = REF("optTextRecord")

  // called by makeCaseClassWithType and buildSeqParser
  def buildParser(tagged: Tagged[_], mixed: Boolean, wrapInDataRecord: Boolean): Tree =
    buildParser(tagged, Param(tagged).occurrence, mixed, wrapInDataRecord)

  def buildParser(particle: Tagged[_], occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): Tree = particle match {
    case tagged: TaggedLocalElement  => buildElemParser(tagged, occurrence, mixed, wrapInDataRecord, false)
    // case ref: ElemRef             => buildElemParser(buildElement(ref), occurrence, mixed, wrapInDataRecord, false)
    // case ref: GroupRef            => buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)
    // case compositor: HasParticle  => buildCompositorParser(compositor, occurrence, mixed, wrapInDataRecord)
    // case any: AnyDecl             => buildAnyParser(any.namespaceConstraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
    case _ => EmptyTree
  }

  def buildParserTree(tagged: TaggedLocalElement, occurrence: Occurrence): Tree = {
    val elem = tagged.resolve
    buildParserTree(ElemNameClass APPLY (elementNamespaceTree(elem), LIT(elem.name.get)), occurrence)
  }

  def buildParserTree(base: Tree, occurrence: Occurrence): Tree =
    if (occurrence.isMultiple) REF("rep") APPLY base
    else if (occurrence.isOptional) REF("opt") APPLY base
    else PAREN(base)

  def buildElemParser(tagged: TaggedLocalElement, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean,
                      ignoreSubGroup: Boolean): Tree = {
    import Occurrence._

    def buildConverter(typeSymbol: TaggedType[_], occurrence: Occurrence): Tree = {
      // val record = "scalaxb.DataRecord(x.namespace, Some(x.name), " + buildTypeSymbolArg("x", typeSymbol) + ")"
      val record = DataRecordClass APPLY(REF("x") DOT "namespace",
        SOME(REF("x") DOT "name"),
        buildTypeSymbolArg(REF("x"), typeSymbol))

      // val nillableRecord = "scalaxb.DataRecord(x.namespace, Some(x.name), x.nilOption map {" + buildTypeSymbolArg("_", typeSymbol) + "})"
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
          case XsAnySimpleType | XsAnyType => buildAnyParser(Nil, occurrence, mixed, wrapInDataRecord, true)
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

  def buildAnyParser(namespaceConstraint: List[String], occurrence: Occurrence, mixed: Boolean,
                     wrapInDataRecord: Boolean, laxAny: Boolean): Tree = {
    def stack = (ElemNameClass APPLY(REF("node"))) LIST_:: REF("stack")
    def converter: Tree =
      if (occurrence.nillable) buildFromXML(nillableWildCardType, WILDCARD,
        stack, None)
      else buildFromXML(wildCardType, WILDCARD, stack, None)

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
}
