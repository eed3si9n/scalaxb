package scalaxb.compiler.xsd2

trait XMLOutputs { self: Args with Params with Lookup with Namer =>
  import com.weiglewilczek.slf4s.Logger
  import Predef.{any2stringadd => _, _}
  
  private lazy val logger = Logger("xsd.XMLOutput")

  def buildXMLString(param: Param): String = {
    import Occurrence._

    val ns = elementNamespaceString(param.topLevelElement, param.namespace, param.qualified)
    val name = "__obj." + makeParamName(param.name)
    
    val typeAttribute = param.typeSymbol match {
      case AnyLike(_) => "true"
      case _          => "false"
    }
    
    lazy val xToXMLCode = "x => " + buildToXML(param.baseTypeName, "x, x.namespace, x.key, __scope, " + typeAttribute)
    lazy val toXMLCode = param.typeSymbol match {
      case AnyLike(_)                => xToXMLCode
      case x: TaggedDataRecordSymbol => xToXMLCode
      case _ => buildToXML(param.baseTypeName, "_, " + ns + ", " + quote(Some(param.name)) + 
        ", __scope, " + typeAttribute)
    }

    lazy val optionalType = param.baseTypeName.option
    lazy val xOptionalToXMLCode = "x => " + buildToXML(optionalType, "x, x.namespace, x.key, __scope, " + typeAttribute)
    lazy val optionalToXMLCode = param.typeSymbol match {
      case AnyLike(_)                => xOptionalToXMLCode
      case x: TaggedDataRecordSymbol => xOptionalToXMLCode
      case _ => buildToXML(optionalType, "_, " + ns + ", " + quote(Some(param.name)) + ", __scope, " + typeAttribute)
    }

    lazy val singleToXMLCode = param.typeSymbol match {
      case AnyLike(_)                => "Some(" + name + ") map {" + xToXMLCode + "} get"
      case x: TaggedDataRecordSymbol => "Some(" + name + ") map {" + xToXMLCode + "} get"
      case _ => buildToXML(param.baseTypeName, name + ", " + ns + ", " + quote(Some(param.name)) + ", __scope, " + typeAttribute)
    }

    lazy val singleOptionalToXMLCode = param.typeSymbol match {
      case AnyLike(_)                => "Some(" + name + ") map {" + optionalToXMLCode + "} get"
      case x: TaggedDataRecordSymbol => "Some(" + name + ") map {" + optionalToXMLCode + "} get"
      case _ => buildToXML(optionalType, name + ", " + ns + ", " + quote(Some(param.name)) + ", __scope, " + typeAttribute)
    }
    
    val retval = param.occurrence match {
      case UnboundedNillable(_)    => name + " flatMap { " + optionalToXMLCode + " }"
      case UnboundedNotNillable(_) => name + " flatMap { " + toXMLCode + " }"
      case OptionalNillable(_)     => name + " map { " +  optionalToXMLCode + " } getOrElse {Nil}"
      case OptionalNotNillable(_)  => name + " map { " + toXMLCode + " } getOrElse {Nil}"
      case SingleNillable(_)       => singleOptionalToXMLCode
      case SingleNotNillable(_)    => singleToXMLCode
    }
    
    retval
  }
}
