package scalaxb.compiler.xsd2

trait Parsers { self: Namer with Lookup with Args with Params =>
  import Predef.{any2stringadd => _, _}
  import com.weiglewilczek.slf4s.Logger
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsAnySimpleType, XsTypeSymbol, XsInt}
  import Defs._

  private lazy val logger: Logger = Logger("xsd2.Parsers")

  def buildTextParser: String = "optTextRecord"

  // called by makeCaseClassWithType and buildSeqParser
  def buildParser(tagged: Tagged[_], mixed: Boolean, wrapInDataRecord: Boolean): String =
    buildParser(tagged, Param(tagged).occurrence, mixed, wrapInDataRecord)

  def buildParser(particle: Tagged[_], occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): String = particle match {
    case tagged: TaggedLocalElement  => buildElemParser(tagged, occurrence, mixed, wrapInDataRecord, false)
    // case ref: ElemRef             => buildElemParser(buildElement(ref), occurrence, mixed, wrapInDataRecord, false)
    // case ref: GroupRef            => buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)
    // case compositor: HasParticle  => buildCompositorParser(compositor, occurrence, mixed, wrapInDataRecord)
    // case any: AnyDecl             => buildAnyParser(any.namespaceConstraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
    case _ => ""
  }

  def buildParserString(tagged: TaggedLocalElement, occurrence: Occurrence): String = {
    val elem = tagged.resolve
    buildParserString("scalaxb.ElemName(" +
      elementNamespaceString(elem) + ", " +
      quote(elem.name.get) + ")",
      occurrence)
  }

  def buildParserString(base: String, occurrence: Occurrence) =
    if (occurrence.isMultiple) "rep(" + base + ")"
    else if (occurrence.isOptional) "opt(" + base + ")"
    else "(" + base + ")"

  def buildElemParser(tagged: TaggedLocalElement, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean,
                      ignoreSubGroup: Boolean): String = {
    import Occurrence._

    def buildConverter(typeSymbol: Tagged[Any], occurrence: Occurrence): String = {
      val record = "scalaxb.DataRecord(x.namespace, Some(x.name), " + buildArg("x", typeSymbol) + ")"
      val nillableRecord = "scalaxb.DataRecord(x.namespace, Some(x.name), x.nilOption map {" + buildArg("_", typeSymbol) + "})"

      (occurrence) match {
        case UnboundedNillable(_)    => "(_.toSeq map { x => " + nillableRecord + " })"
        case UnboundedNotNillable(_) => "(_.toSeq map { x => " + record + " })"
        case OptionalNillable(_)     => "(_ map { x => " + nillableRecord + " })"
        case OptionalNotNillable(_)  => "(_ map { x => " + record + " })"
        case SingleNillable(_)       => "(x => " + nillableRecord + ")"
        case SingleNotNillable(_)    => "(x => " + record + ")"
      }
    }

    def addConverter(p: String): String =
      if (wrapInDataRecord) "(" + p + " ^^ " + NL +
        indent(3) + buildConverter(tagged.typeStructure, occurrence) + ")"
      else p

    // if (isSubstitionGroup(elem) && !ignoreSubGroup) addConverter(buildSubstitionGroupParser(elem, occurrence, mixed))
    // else tagged.typeStructure match { ...

    tagged.typeStructure match {
      case x: TaggedSymbol =>
        x.value match {
          case XsAnySimpleType | XsAnyType => buildAnyParser(Nil, occurrence, mixed, wrapInDataRecord, true)
          case symbol: BuiltInSimpleTypeSymbol => addConverter(buildParserString(tagged, occurrence))
        }

      case TaggedSimpleType(_, _) | TaggedComplexType(_, _) =>  addConverter(buildParserString(tagged, occurrence))

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
                     wrapInDataRecord: Boolean, laxAny: Boolean): String = {
    val converter =
      if (occurrence.nillable) buildFromXML(QualifiedName(Some(SCALAXB_URI), "DataRecord[Option[Any]]"), "_",
        "scalaxb.ElemName(node) :: stack", None)
      else buildFromXML(wildCardTypeName, "_", "scalaxb.ElemName(node) :: stack", None)
    val parser = "any(%s)".format(
      if (laxAny) "_ => true"
      else namespaceConstraint match {
        case Nil => "_ => true"
        case "##any" :: Nil => "_ => true"
        case "##other" :: Nil => "_.namespace != %s" format (quoteUri(schema.targetNamespace))
        case _ =>
          """x => %s contains x.namespace""" format (namespaceConstraint.map {
            case "##targetNamespace" => quoteUri(schema.targetNamespace)
            case "##local" => "None"
            case x => "Some(%s)".format(x)
          }).mkString("List(", ", ", ")")
      })

    buildParserString(if (mixed) "((" + parser + " ^^ (" + converter + ")) ~ " + NL +
        indent(3) + buildTextParser + ") ^^ " + NL +
        indent(3) + "{ case p1 ~ p2 => Seq.concat(Seq(p1), p2.toList) }"
      else if (wrapInDataRecord) "(" + parser + " ^^ (" + converter + "))"
      else parser,
      occurrence)
  }
}
