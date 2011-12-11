package scalaxb.compiler.xsd2

trait Args { self: Namer with Lookup with Params =>
  import com.weiglewilczek.slf4s.Logger
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt, XsAnySimpleType}
  import Defs._
  import Occurrence._
  import Predef.{any2stringadd => _, _}
  import xmlschema._
  
  private lazy val logger = Logger("xsd2.Args")

  def buildFromXML(typeName: QualifiedName): String = "scalaxb.fromXML[" + typeName.fullyQualifiedName + "]"
  def buildFromXML(typeName: QualifiedName, selector: String, stackString: String, formatter: Option[String]): String =
    buildFromXML(typeName) + "(%s, %s)%s".format(selector, stackString,
      formatter map {"(" + _ + ")"} getOrElse {""})

  def buildToXML(typeName: QualifiedName, args: String): String =
    "scalaxb.toXML[" + typeName.fullyQualifiedName + "](" + args + ")"

  // called by buildConverter
  def buildTypeSymbolArg(selector: String, typeSymbol: Tagged[Any]): String = typeSymbol match {
    case x: TaggedWildCard => selector
    case x: TaggedSymbol =>
      x.value match {
        case XsAnySimpleType | XsAnyType => selector
        case symbol: BuiltInSimpleTypeSymbol => buildTypeSymbolArg(buildTypeName(x), selector, SingleNotNillable())
      }
    case x: TaggedSimpleType => buildTypeSymbolArg(buildTypeName(baseType(x)), selector, SingleNotNillable())
    case x: TaggedComplexType =>
      buildFromXML(buildTypeName(x), selector, "scalaxb.ElemName(node) :: stack", None)
  }

  def buildTypeSymbolArg(typeName: QualifiedName, selector: String, occurrence: Occurrence,
      defaultValue: Option[String] = None, fixedValue: Option[String] = None,
      wrapForLongAll: Boolean = false, formatter: Option[String] = None): String = {
    import Occurrence._

    val stack = "scalaxb.ElemName(node) :: stack"
    def fromSelector = buildFromXML(typeName, selector, stack, formatter)
    def fromU = buildFromXML(typeName, "_", stack, formatter)
    def fromValue(x: String) = buildFromXML(typeName, "scala.xml.Text(" + quote(x) + ")", stack, formatter)

    val retval = if (wrapForLongAll) {
      // PrefixedAttribute only contains pre, so you need to pass in node to get the namespace.
      if (selector.contains("@")) selector + ".headOption map { x => scalaxb.DataRecord(x, node, " +
        buildFromXML(typeName, "x", stack, formatter) + ") }"
      else selector + ".headOption map { x => scalaxb.DataRecord(x, " +
        buildFromXML(typeName, "x", stack, formatter) + ") }"
    } else occurrence match {
      case UnboundedNillable(_)    => selector + ".toSeq map { _.nilOption map { " + fromU + " }}"
      case UnboundedNotNillable(_) => selector + ".toSeq map { " + fromU + " }"
      case OptionalNillable(_)     => selector + ".headOption map { _.nilOption map { " + fromU + " }}"
      case OptionalNotNillable(_)  => selector + ".headOption map { " + fromU + " }"
      case SingleNillable(_) | SingleNotNillable(_) =>
        (occurrence.nillable, defaultValue, fixedValue) match {
          case ( _, _, Some(x)) => fromValue(x)
          case (_, Some(x), _)  => selector + ".headOption map { " + fromU + " } getOrElse { " + fromValue(x) + " }"
          case (true, _, _)     => selector + ".nilOption map { " + fromU + " }"
          case (false, _, _)    => fromSelector
        }
    }

    retval
  }

  def buildArg(tagged: Tagged[Any], pos: Int): String = buildArg(tagged, buildSelector(pos), false)

  def buildArg(tagged: Tagged[Any], selector: String, wrapForLongAll: Boolean): String =
    // if ((isSubstitionGroup(elem))) selector
    tagged match {
      case x: TaggedSymbol =>
        buildTypeSymbolArg(buildTypeName(x), selector, SingleNotNillable(), None, None, wrapForLongAll)
      case x: TaggedSimpleType =>
        buildTypeSymbolArg(buildTypeName(x), selector, SingleNotNillable(), None, None, wrapForLongAll)
      case elem: TaggedLocalElement if elem.isSubstitutionGroup => selector
      case tagged: TaggedLocalElement =>
        val o = Occurrence(tagged)
        val elem = tagged.resolve
        elem.typeStructure match {
          case x: TaggedSymbol =>
            implicit val tag = x.tag
            x.value match {
              case XsAnySimpleType | XsAnyType =>
                buildTypeSymbolArg(
                  if (o.nillable) QualifiedName.DataRecordOptionAnyTypeName
                  else buildTypeName(x), selector, o.copy (nillable = false),
                  elem.default, elem.fixed, wrapForLongAll)
              case symbol: BuiltInSimpleTypeSymbol =>
                buildTypeSymbolArg(buildTypeName(symbol), selector, o, elem.default, elem.fixed, wrapForLongAll)
            }
          case tagged: TaggedSimpleType =>
            buildTypeSymbolArg(buildTypeName(tagged), selector, o, elem.default, elem.fixed, wrapForLongAll)
          case tagged: TaggedComplexType =>
            buildTypeSymbolArg(buildTypeName(tagged), selector, o, elem.default, elem.fixed, wrapForLongAll)
        }
      case x: TaggedKeyedGroup =>
        val param = Param(x)
        param.occurrence match {
          case Multiple(_)         => selector + ".toSeq"
          case OptionalNillable(_) => selector + " getOrElse {None}"
          case _ => selector
        }
      case AnyLike(x) =>
        val param = Param(x)
        buildTypeSymbolArg(buildTypeName(x), selector, param.occurrence, None, None, wrapForLongAll)
      case _ => error("Args#buildArg: " + tagged.toString)
    }

  // called by generateDefaultFormat. By spec, <all> contains only elements.
  def buildArgForAll(tagged: Tagged[Any]): String = {
    val o = tagged match {
      case elem: TaggedLocalElement => elemToOptional(elem)
      case _ => error("buildArgForAll unsupported type: " + tagged.toString)
    }
    "%s map { %s -> _ }" format(buildArg(o, buildSelector(o), true), quote(buildNodeName(o, true)))
  }

  def elemToOptional(tagged: TaggedLocalElement): TaggedLocalElement =
    tagged.copy(value = tagged.value match {
      case elem: XLocalElement => elem.copy(minOccurs = 0, maxOccurs = "1")
    })

  def buildArgForMixed(tagged: Tagged[Any], pos: Int): String =
    buildArgForMixed(tagged, buildSelector(pos))

  def buildArgForMixed(tagged: Tagged[Any], selector: String): String = {
    import Occurrence._

    val occcurrence = Param(tagged).occurrence
    val isCompositor = tagged match {
      case x: TaggedWildCard => true
      case x: TaggedKeyedGroup => true
      case elem: Tagged[XElement] =>
        elem.typeStructure match {
          case x: TaggedSymbol =>
            x.value match {
              case XsAnySimpleType | XsAnyType => true
              case symbol: BuiltInSimpleTypeSymbol => false
            }
          case _ => false
        }
      case _ => false
    }

    val retval = occcurrence match {
      case Multiple(o) =>
        if (isCompositor) selector + ".flatten"
        else selector
      case Optional(o) =>
        if (isCompositor) selector + " getOrElse {Nil}"
        else selector + ".toList"
      case Single(o) =>
        if (isCompositor) selector
        else "Seq(" + selector + ")"
    }

    logger.debug("buildArgForMixed: " + occcurrence.toString + ": " + tagged.toString + ": " + retval)
    retval
  }

  def buildArgForOptTextRecord(pos: Int): String =
    buildSelector(pos) + ".toList"

  def buildSelector(pos: Int): String = "p" + (pos + 1)
  def buildSelector(nodeName: String): String = "(node \\ \"" + nodeName + "\")"
  def buildSelector(tagged: Tagged[XElement]): String = buildSelector(buildNodeName(tagged, false))

  // scala's <foo/> \ "foo" syntax is not namespace aware, but {ns}foo is useful for long all.
  def buildNodeName(tagged: Tagged[XElement], prependNamespace: Boolean): String =
    tagged.resolve match {
      case elem: TaggedTopLevelElement =>
        elem.tag.namespace match {
          case None => elem.name.get
          case Some(ns) =>
            if (prependNamespace) "{%s}".format(ns) + elem.name.get
            else elem.name.get
        }
      case elem: TaggedLocalElement => elem.name.get
    }

}
