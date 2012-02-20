package scalaxb.compiler.xsd2

trait XMLOutputs { self: Args with Params with Lookup with Namer =>
  import com.weiglewilczek.slf4s.Logger
  import Predef.{any2stringadd => _, _}
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private lazy val logger = Logger("xsd.XMLOutput")

  def buildXMLTree(param: Param): Tree = {
    import Occurrence._

    val ns = elementNamespaceTree(param.topLevelElement, param.namespace, param.qualified)
    val name: Tree = REF("__obj") DOT makeParamName(param.name)
    
    val typeAttribute = param.typeSymbol match {
      case AnyLike(_) => TRUE
      case _          => FALSE
    }

    val toXMLArgs: List[Tree] = REF("x") :: (REF("x") DOT "namespace").tree ::
      (REF("x") DOT "key").tree :: REF("__scope") :: typeAttribute :: Nil
    val uToXMLArgs = REF("x") :: ns :: optionTree(Some(param.name)) :: REF("__scope") :: typeAttribute :: Nil

    lazy val xToXMLCode = LAMBDA(PARAM("x")) ==> BLOCK(buildToXML(param.baseTypeName, toXMLArgs))
    lazy val xOptionalToXMLCode: Tree = LAMBDA(PARAM("x"))  ==> BLOCK(buildToXML(optionalType, toXMLArgs))


    lazy val toXMLCode: Tree = param.typeSymbol match {
      case AnyLike(_)                => xToXMLCode
      case x: TaggedDataRecordSymbol => xToXMLCode
      case _ => LAMBDA(PARAM("x")) ==> BLOCK(buildToXML(param.baseTypeName, uToXMLArgs))
    }

    lazy val optionalType = param.baseTypeName.option

    lazy val optionalToXMLCode: Tree = param.typeSymbol match {
      case AnyLike(_)                => xOptionalToXMLCode
      case x: TaggedDataRecordSymbol => xOptionalToXMLCode
      case _ => LAMBDA(PARAM("x")) ==> BLOCK(buildToXML(optionalType, uToXMLArgs))
    }

    lazy val singleToXMLCode: Tree = param.typeSymbol match {
      case AnyLike(_)                => SOME(name) MAP xToXMLCode POSTFIX("get")
      case x: TaggedDataRecordSymbol => SOME(name) MAP xToXMLCode POSTFIX("get")
      case _ => buildToXML(param.baseTypeName, name :: ns :: optionTree(Some(param.name)) :: REF("__scope") :: typeAttribute :: Nil)
    }

    lazy val singleOptionalToXMLCode: Tree = param.typeSymbol match {
      case AnyLike(_)                => SOME(name) MAP optionalToXMLCode POSTFIX("get")
      case x: TaggedDataRecordSymbol => SOME(name) MAP optionalToXMLCode POSTFIX("get")
      case _ =>
        buildToXML(optionalType, name :: ns :: optionTree(Some(param.name)) :: REF("__scope") :: typeAttribute :: Nil)
    }
    
    val retval: Tree = param.occurrence match {
      case UnboundedNillable(_)    => name FLATMAP optionalToXMLCode
      case UnboundedNotNillable(_) => name FLATMAP toXMLCode
      case OptionalNillable(_)     => name MAP optionalToXMLCode INFIX("getOrElse") APPLY BLOCK(NIL)
      case OptionalNotNillable(_)  => name MAP toXMLCode INFIX("getOrElse") APPLY BLOCK(NIL)
      case SingleNillable(_)       => singleOptionalToXMLCode
      case SingleNotNillable(_)    => singleToXMLCode
    }
    
    retval
  }
}
