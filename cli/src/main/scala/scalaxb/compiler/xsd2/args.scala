package scalaxb.compiler.xsd2

trait Args { self: Namer with Lookup with Params =>
  import com.weiglewilczek.slf4s.Logger
  import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt, XsAnySimpleType}
  import Defs._
  import Occurrence._
  import Predef.{any2stringadd => _, _}
  
  private lazy val logger = Logger("xsd2.Args")

  def buildFromXML(typeName: QualifiedName): String = "scalaxb.fromXML[" + typeName.toScalaCode + "]"
  def buildFromXML(typeName: QualifiedName, selector: String, stackString: String, formatter: Option[String]): String =
    buildFromXML(typeName) + "(%s, %s)%s".format(selector, stackString,
      formatter map {"(" + _ + ")"} getOrElse {""})

  // called by buildConverter
  def buildArg(selector: String, tagged: Tagged[Any]): String = tagged match {
    case x: TaggedWildCard => selector
    case x: TaggedSymbol =>
      x.value match {
        case XsAnySimpleType | XsAnyType => selector
        case symbol: BuiltInSimpleTypeSymbol => buildArg(buildTypeName(x), selector, SingleNotNillable())
      }
    case x: TaggedSimpleType => buildArg(buildTypeName(baseType(x)), selector, SingleNotNillable())
    case x: TaggedComplexType =>
      buildFromXML(buildTypeName(x), selector, "scalaxb.ElemName(node) :: stack", None)
  }

  def buildArg(typeName: QualifiedName, selector: String, occurrence: Occurrence,
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
}
