package scalaxb.compiler.xsd2

trait Args { self: Namer with Lookup with Params with Symbols =>
  import scalashim._
  import com.codahale.logula.Log
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt, XsAnySimpleType}
  import Defs._
  import Occurrence._
  import Predef.{any2stringadd => _, _}
  import xmlschema._
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
  
  private val logger = Log.forName("xsd2.Args")

  def stackTree = (ElemNameClass APPLY(REF("node"))) LIST_:: REF("stack")

  def buildFromXML(typ: Type): Tree =
    Scalaxb_fromXML APPLYTYPE typ
  def buildFromXML(typ: Type, selector: Tree, stackTree: Tree, formatter: Option[Tree]): Tree = {
    val tree = buildFromXML(typ) APPLY(selector, stackTree)
    formatter map {tree APPLY _} getOrElse {tree}
  }
  def buildToXML(typ: Type, args: List[Tree]): Tree =
    Scalaxb_toXML APPLYTYPE typ APPLY(args)

  // called by buildConverter
  def buildTypeSymbolArg(selector: Tree, typeSymbol: TaggedType[_]): Tree = typeSymbol match {
    case x: TaggedSymbol =>
      x.value match {
        case XsAnySimpleType | XsAnyType => selector
        case symbol: BuiltInSimpleTypeSymbol => buildTypeSymbolArg(buildType(x), selector, SingleNotNillable())
      }
    case x: TaggedSimpleType => buildTypeSymbolArg(buildType(x), selector, SingleNotNillable())
    case x: TaggedComplexType =>
      buildFromXML(buildType(x), selector, stackTree, None)
  }

  def buildTypeSymbolArg(typ: Type, selector: Tree, occurrence: Occurrence,
      defaultValue: Option[String] = None, fixedValue: Option[String] = None,
      wrapForLongAll: Boolean = false, formatter: Option[Tree] = None): Tree = {
    import Occurrence._

    def fromSelector = buildFromXML(typ, selector, stackTree, formatter)
    def fromX = buildFromXML(typ, REF("x"), stackTree, formatter)
    def lambdaX = LAMBDA(PARAM("x")) ==> BLOCK(fromX)
    def fromValue(x: String) = buildFromXML(typ, TextClass APPLY LIT(x), stackTree, formatter)

    val retval: Tree = if (wrapForLongAll) {
      // PrefixedAttribute only contains pre, so you need to pass in node to get the namespace.
      if (treeToString(selector).contains("@")) (selector DOT "headOption") MAP LAMBDA(PARAM("x", NodeClass)) ==> BLOCK(
          DataRecordClass APPLY(REF("x"), REF("node"), buildFromXML(typ, REF("x"), stackTree, formatter))
        )
      else (selector DOT "headOption") MAP LAMBDA(PARAM("x", NodeClass)) ==> BLOCK(
            DataRecordClass APPLY(REF("x"), buildFromXML(typ, REF("x"), stackTree, formatter))
        )
    } else occurrence match {
      case UnboundedNillable(_)    => (selector DOT "toSeq") MAP BLOCK((WILDCARD DOT "nilOption") MAP lambdaX)
      case UnboundedNotNillable(_) => (selector DOT "toSeq") MAP lambdaX
      case OptionalNillable(_)     => (selector DOT "headOption") MAP BLOCK((WILDCARD DOT "nilOption") MAP lambdaX)
      case OptionalNotNillable(_)  => (selector DOT "headOption") MAP lambdaX
      case SingleNillable(_) | SingleNotNillable(_) =>
        (occurrence.nillable, defaultValue, fixedValue) match {
          case ( _, _, Some(x)) => fromValue(x)
          case (_, Some(x), _)  => (selector DOT "headOption") MAP lambdaX INFIX("getOrElse") APPLY BLOCK(fromValue(x))
          case (true, _, _)     => (selector DOT "nilOption") MAP lambdaX
          case (false, _, _)    => fromSelector
        }
    }

    retval
  }

  def buildSimpleContentArg(tagged: Tagged[Any]): Tree =
    buildArg(tagged, REF("node"), false)

  def buildArg(tagged: Tagged[Any], pos: Int): Tree = buildArg(tagged, buildSelector(pos), false)

  def buildArg(tagged: Tagged[Any], selector: Tree, wrapForLongAll: Boolean): Tree =
    // if ((isSubstitionGroup(elem))) selector
    tagged match {
      case x: TaggedSymbol =>
        buildTypeSymbolArg(buildType(x), selector, SingleNotNillable(), None, None, wrapForLongAll)
      case x: TaggedSimpleType =>
        buildTypeSymbolArg(buildType(x), selector, SingleNotNillable(), None, None, wrapForLongAll)
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
                  if (o.nillable) DataRecordOptionAnyClass
                  else buildType(x), selector, o.copy (nillable = false),
                  elem.default, elem.fixed, wrapForLongAll)
              case symbol: BuiltInSimpleTypeSymbol =>
                buildTypeSymbolArg(buildType(symbol), selector, o, elem.default, elem.fixed, wrapForLongAll)
            }
          case tagged: TaggedSimpleType =>
            buildTypeSymbolArg(buildType(tagged), selector, o, elem.default, elem.fixed, wrapForLongAll)
          case tagged: TaggedComplexType =>
            buildTypeSymbolArg(buildType(tagged), selector, o, elem.default, elem.fixed, wrapForLongAll)
        }
      case x: TaggedGroupRef =>
        val param = Param(x)
        param.occurrence match {
          case Multiple(_)         => selector DOT "toSeq"
          case OptionalNillable(_) => selector INFIX("getOrElse") APPLY BLOCK(NONE)
          case _ => selector
        }
      case x: TaggedKeyedGroup =>
        val param = Param(x)
        param.occurrence match {
          case Multiple(_)         => selector DOT "toSeq"
          case OptionalNillable(_) => selector INFIX("getOrElse") APPLY BLOCK(NONE)
          case _ => selector
        }
      case AnyLike(x) =>
        val param = Param(x)
        buildTypeSymbolArg(buildType(x), selector, param.occurrence, None, None, wrapForLongAll)
      case _ => sys.error("Args#buildArg: " + tagged.toString)
    }

  // called by generateDefaultFormat. By spec, <all> contains only elements.
  def buildArgForAll(tagged: Tagged[Any]): Tree = {
    val o = tagged match {
      case elem: TaggedLocalElement => elemToOptional(elem)
      case _ => sys.error("buildArgForAll unsupported type: " + tagged.toString)
    }
    // "%s map { %s -> _ }" format(buildArg(o, buildSelector(o), true), quote(buildNodeName(o, true)))
    PAREN(buildArg(o, buildSelector(o), true)) MAP BLOCK( LIT(buildNodeName(o, true)) ANY_-> WILDCARD )
  }

  def elemToOptional(tagged: TaggedLocalElement): TaggedLocalElement =
    tagged.copy(value = tagged.value match {
      case elem: XLocalElement => elem.copy(minOccurs = 0, maxOccurs = "1")
    })

  def buildArgForMixed(tagged: Tagged[Any], pos: Int): Tree =
    buildArgForMixed(tagged, buildSelector(pos))

  def buildArgForMixed(tagged: Tagged[Any], selector: Tree): Tree = {
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

    val retval: Tree = occcurrence match {
      case Multiple(o) =>
        if (isCompositor) selector DOT "flatten"
        else selector
      case Optional(o) =>
        if (isCompositor) selector INFIX("getOrElse") APPLY BLOCK(NIL)
        else selector DOT "toList"
      case Single(o) =>
        if (isCompositor) selector
        else SEQ(selector)
    }

    logger.debug("buildArgForMixed: " + occcurrence.toString + ": " + tagged.toString + ": " + retval)
    retval
  }

  def buildArgForOptTextRecord(pos: Int): Tree =
    buildSelector(pos) DOT "toList"

  def buildSelector(pos: Int): Tree = REF("p" + (pos + 1))
  def buildSelector(nodeName: String): Tree = PAREN(REF("node") INFIX("\\") APPLY LIT(nodeName))
  def buildSelector(tagged: Tagged[XElement]): Tree = buildSelector(buildNodeName(tagged, false))

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
