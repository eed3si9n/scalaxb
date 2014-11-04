/*
 * Copyright (c) 2011 e.e d3si9n
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

package scalaxb.compiler.wsdl11

trait GenSource {
  import scalashim._
  import wsdl11._
  import masked.scalaxb.{DataRecord}
  import scalaxb.compiler.{Config, Snippet, ReferenceNotFound, Module, Log, ScalaNames}
  import Module.{NL, indent, camelCase}
  import scala.xml.Node
  import scalaxb.compiler.xsd.{ReferenceTypeSymbol, SimpleTypeDecl, ComplexTypeDecl, BuiltInSimpleTypeSymbol,
    XsTypeSymbol, AnyType, XsAnyType, Single, Multiple, Optional, Cardinality }

  val WSDL_SOAP11 = "http://schemas.xmlsoap.org/wsdl/soap/"
  val WSDL_SOAP12 = "http://schemas.xmlsoap.org/wsdl/soap12/"
  val WSDL_HTTP = "http://schemas.xmlsoap.org/wsdl/http"
  val SOAP_MEP_REQUEST_RESPONSE = "http://www.w3.org/2003/05/soap/mep/request-response"
  val SOAP_MEP_SOAP_RESPONSE = "http://www.w3.org/2003/05/soap/mep/soap-response"
  val encodedErrorMessage = "rpc/encoded wsdls are not supported in scalaxb"

  private val logger = Log.forName("wsdl.GenSource")
  def config: Config
  def context: WsdlContext
  def scope: scala.xml.NamespaceBinding
  def schemas = context.xsdcontext.schemas.toList
  def xsdgenerator: scalaxb.compiler.xsd.GenSource
  lazy val targetNamespace: Option[String] = xsdgenerator.schema.targetNamespace
  lazy val pkg = xsdgenerator.packageName(targetNamespace, xsdgenerator.context)
  lazy val scalaNames: ScalaNames = new ScalaNames {}

  sealed trait SoapBindingStyle {}
  case object DocumentStyle extends SoapBindingStyle
  case object RpcStyle extends SoapBindingStyle

  // this is the top-level entry point for wsdl code generation.
  def generate(definition: XDefinitionsType, bindings: Seq[XBindingType]): Snippet = {
    logger.debug("generate")
    Snippet(
      (soap11Bindings(bindings) map {makeSoap11Binding}) ++
      (soap12Bindings(bindings) map {makeSoap12Binding}): _*)
  }

  def soap12Bindings(bindings: Seq[XBindingType]) =
    bindings filter { binding =>
      binding.any exists {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP12
        case _ => false
      }
    }

  def soap11Bindings(bindings: Seq[XBindingType]) =
    if (!soap12Bindings(bindings).isEmpty) Nil
    else bindings filter { binding =>
      binding.any exists {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP11
        case _ => false
      }
    }

  // called by generate.
  def makeSoap11Binding(binding: XBindingType): Snippet = {
    val name = makeBindingName(binding)
    logger.debug("makeSoap11Binding: " + name)

    val interfaceType = context.interfaces(splitTypeName(binding.typeValue))
    val interfaceTypeName = interfaceType.name.capitalize
    val interfaceTypeFQN = xsdgenerator.buildFullyQualifiedNameFromPackage(pkg, interfaceTypeName)
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP11 =>
        Some((node \ "@location").text)
      case _ => None
    }}

    val soapBindingStyle = parseSoapBindingStyle(binding.any.headOption, DocumentStyle)
    val addressString = address map {"""def baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val operationOutputs = binding.operation flatMap { makeOperationOutput(_, interfaceType, soapBindingStyle, false) }
    val operations = binding.operation map { opBinding => makeOperation(opBinding, interfaceType, soapBindingStyle, false) }
    val bindingOps = binding.operation map { opBinding => makeSoapOpBinding(opBinding, interfaceType, soapBindingStyle, false) }
    val importFutureString = if (config.async) "import scala.concurrent.Future" + NL else ""
    val clientTraitName = if (config.async) "Soap11ClientsAsync" else "Soap11Clients"

    val interfaceTrait = <source>
{importFutureString}

trait {interfaceTypeName} {{
  {operations.mkString(NL + "  ")}
}}

{operationOutputs.mkString(NL + NL)}
</source>

    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.{clientTraitName} =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(targetNamespace) }
    lazy val service: {interfaceTypeFQN} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeFQN} {{
      import scalaxb.ElemName._
      {bindingOps.mkString(NL + "      ")}
    }}
  }}
</source>
    Snippet(interfaceTrait, <source/>, bindingTrait, <source/>)
  }

  // http://www.w3.org/TR/2007/REC-wsdl20-adjuncts-20070626/#soap-binding
  // http://www.w3.org/TR/2007/REC-soap12-part2-20070427/
  def makeSoap12Binding(binding: XBindingType): Snippet = {
    val name = makeBindingName(binding)
    val interfaceType = context.interfaces(splitTypeName(binding.typeValue))
    val interfaceTypeName = interfaceType.name.capitalize
    val interfaceTypeFQN = xsdgenerator.buildFullyQualifiedNameFromPackage(pkg, interfaceTypeName)
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP12 =>
        Some((node \ "@location").text)
      case _ => None
    }}

    val soapBindingStyle = parseSoapBindingStyle(binding.any.headOption, DocumentStyle)
    val addressString = address map {"""def baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val operationOutputs = binding.operation flatMap { makeOperationOutput(_, interfaceType, soapBindingStyle, true) }
    val operations = binding.operation map { opBinding => makeOperation(opBinding, interfaceType, soapBindingStyle, true) }
    val bindingOps = binding.operation map { opBinding => makeSoapOpBinding(opBinding, interfaceType, soapBindingStyle, true) }
    val importFutureString = if (config.async) "import scala.concurrent.Future" + NL else ""
    val clientTraitName = if (config.async) "SoapClientsAsync" else "SoapClients"

    val interfaceTrait = <source>
{importFutureString}

trait {interfaceTypeName} {{
  {operations.mkString(NL + "  ")}
}}

{operationOutputs.mkString(NL + NL)}
</source>

    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.{clientTraitName} =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(targetNamespace) }
    lazy val service: {interfaceTypeFQN} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeFQN} {{
      import scalaxb.ElemName._
      {bindingOps.mkString(NL + "      ")}
    }}
  }}
</source>
    Snippet(interfaceTrait, <source/>, bindingTrait, <source/>)
  }

  // generate case class for op binding in case the soap header is bound.
  def makeOperationOutput(binding: XBinding_operationType, intf: XPortTypeType,
                          defaultSoapBindingStyle: SoapBindingStyle, soap12: Boolean): Option[String] = {
    val op = boundOperation(binding, intf)
    val soapBindingStyle = parseSoapBindingStyle(binding.any.headOption, defaultSoapBindingStyle)
    val outputOpt: Option[XParamType] = operationParts(op)._2

    def makeOperationOutputArgs: Seq[ParamCache] = {
      val output: XParamType = outputOpt.getOrElse {sys.error("expected ouput: " + op.name)}
      val (headerParts, bodyParts) = splitParamToParts(output, binding.output)
      (bodyParts map toParamCache) ++ (headerParts map toParamCache)
    }
    outputOpt flatMap { output: XParamType =>
      isMultiPart(output, binding.output) map { _ =>
        "case class %s(%s)" format(
          makeOperationOutputWrapperName(op),
          makeOperationOutputArgs map {_.toScalaCode} mkString(", ")
        )
      }
    }
  }

  // return type of an operation
  def outputTypeName(binding: XBinding_operationType, op: XOperationType,
                     output: XParamType, soapBindingStyle: SoapBindingStyle): String =
    if (isMultiPart(output, binding.output)) xsdgenerator.buildFullyQualifiedNameFromPackage(pkg, makeOperationOutputWrapperName(op))
    else {
      singleOutputType(output, soapBindingStyle) map { elem =>
        val param = xsdgenerator.buildParam(elem)
        param.typeName
      } getOrElse {
        val parts = paramMessage(output).part
        if (parts.isEmpty) "Unit"
        else toParamCache(parts.head).typeName
      }
    }

  def isMultiPart(param: XParamType, bindingOption: Option[XStartWithExtensionsTypable]): Boolean =
    (paramMessage(param).part.size > 1) ||
    (!headerBindings(bindingOption).isEmpty)

  def isEmptyPart(param: XParamType, bindingOption: Option[XStartWithExtensionsTypable]): Boolean =
    paramMessage(param).part.isEmpty && headerBindings(bindingOption).isEmpty

  // generate method signature
  def makeOperation(binding: XBinding_operationType, intf: XPortTypeType,
                    defaultSoapBindingStyle: SoapBindingStyle, soap12: Boolean): String = {
    val op = boundOperation(binding, intf)
    val soapBindingStyle = parseSoapBindingStyle(binding.any.headOption, defaultSoapBindingStyle)

    def arg(input: XParamType): String =
      soapBindingStyle match {
        case DocumentStyle =>
          if (isEmptyPart(input, binding.input)) ""
          else if (!isMultiPart(input, binding.input)) buildIRIStyleArgs(input) map {_.toScalaCode} mkString(", ")
          else makeOperationInputArgs(binding, intf) map {_.toScalaCode} mkString(", ")
        case RpcStyle => 
          buildRPCStyleArgs(input) map {_.toScalaCode} mkString(", ")
      }
    val name = escapeKeyWord(camelCase(op.name))
    logger.debug("makeOperation: " + name)

    val retval = (op.xoperationtypeoption, config.async) match {
      case (DataRecord(_, _, XOnewayoperationSequence(input)), _) =>
        "def %s(%s): Unit".format(name, arg(input))

      case (DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)), true) =>
        "def %s(%s): Future[%s]".format(name, arg(input),
          outputTypeName(binding, op, output, soapBindingStyle))

      case (DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)), false) =>
        "def %s(%s): Either[%s, %s]".format(name, arg(input),
          faultsToTypeName(faults, soap12), outputTypeName(binding, op, output, soapBindingStyle))

      case (DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)), true) =>
        "def %s(%s): Future[%s]".format(name, arg(input),
          outputTypeName(binding, op, output, soapBindingStyle))

      case (DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)), false) =>
        "def %s(%s): Either[%s, %s]".format(name, arg(input),
          faultsToTypeName(faults, soap12), outputTypeName(binding, op, output, soapBindingStyle))

      case (DataRecord(_, _, XNotificationoperationSequence(output)), true) =>
        "def %s: Future[%s]".format(name, outputTypeName(binding, op, output, soapBindingStyle))

      case (DataRecord(_, _, XNotificationoperationSequence(output)), false) =>
        "def %s: %s".format(name, outputTypeName(binding, op, output, soapBindingStyle))

      case _ => sys.error("unsupported.")
    }

    logger.debug("makeOperation: " + retval)
    retval
  }

  def operationParts(op: XOperationType): (Option[XParamType], Option[XParamType], Option[Seq[XFaultType]]) =
    op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) => (Some(input), None, None)
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        (Some(input), Some(output), Some(faults))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        (Some(input), Some(output), Some(faults))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        (None, Some(output), None)
      case _ => sys.error("unsupported.")
    }

  def makeOperationOutputWrapperName(op: XOperationType): String =
    xsdgenerator.makeTypeName(op.name + "Output")

  def splitParamToParts(paramType: XParamType, paramBinding: Option[XStartWithExtensionsTypable]): (Seq[XPartType], Seq[XPartType]) = {
    val headers = headerBindings(paramBinding)
    val headerPartNames = (headers map { _.part }).toSet
    val parts = paramMessage(paramType).part
    val (headerParts, bodyParts) = parts partition { p => headerPartNames(p.name.getOrElse("")) }
    (headerParts, bodyParts)
  }

  def makeOperationInputArgs(binding: XBinding_operationType, intf: XPortTypeType): Seq[ParamCache] = {
    val op = boundOperation(binding, intf)
    val input: XParamType = operationParts(op)._1.getOrElse { sys.error("expected input:" + op.name) }
    val (headerParts, bodyParts) = splitParamToParts(input, binding.input)
    (bodyParts map toParamCache) ++ (headerParts map toParamCache)
  }

  def boundOperation(binding: XBinding_operationType, intf: XPortTypeType) =
    (intf.operation filter {_.name == binding.name}).headOption getOrElse {
      sys.error("operation %s was not found in %s".format(binding.name, intf.name))
    }

  def parseSoapBindingStyle(anyHeadOption: Option[DataRecord[Any]], defaultSoapBindingStyle: SoapBindingStyle): SoapBindingStyle =
    anyHeadOption match {
      case Some(DataRecord(_, _, node: Node)) =>
        (node \ "@style").headOption match {
          case Some(x) if x.toString == "document" => DocumentStyle
          case Some(x) if x.toString == "rpc"      => RpcStyle
          case _                                   => defaultSoapBindingStyle
        }
      case _ => defaultSoapBindingStyle
    }

  // generate method impl
  def makeSoapOpBinding(binding: XBinding_operationType, intf: XPortTypeType,
                           defaultSoapBindingStyle: SoapBindingStyle, soap12: Boolean): String = {
    val op = boundOperation(binding, intf)
    logger.debug("makeSoap12OpBinding: " + op.name)

    val address = "baseAddress"
    val quotedMethod = "\"POST\""
    val action = binding.any.headOption match {
      case Some(DataRecord(_, _, node: Node)) => (node \ "@soapAction").headOption map {_.text}
      case _ => None
    }
    val soapBindingStyle = parseSoapBindingStyle(binding.any.headOption, defaultSoapBindingStyle)
    val actionString = action map {"Some(new java.net.URI(\"%s\"))".format(_)} getOrElse {"None"}

    def faultString(faults: Seq[XFaultType]): String =
      faultsToFaultParamTypeName(faults) match {
        case ("Any", _) => "x"
        case (x, true)  => s"""x.asNillableFault[$x]"""
        case (x, _)     => s"""x.asFault[$x]"""
      }
    def faultTypeName: String =
      if (soap12) "scalaxb.Fault[_]"
      else "scalaxb.Soap11Fault[_]"

    val opImpl = (op.xoperationtypeoption, config.async) match {
      case (DataRecord(_, _, XOnewayoperationSequence(input)), true) =>
        // "def %s(%s): Unit".format(op.name, arg(input))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s).map({ case x => () })""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString)

      case (DataRecord(_, _, XOnewayoperationSequence(input)), false) =>
        // "def %s(%s): Unit".format(op.name, arg(input))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => sys.error(x.toString)
          |          case Right(x) => ()
          |        }""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString)

      case (DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)), true) =>
        // "def %s(%s): Option[scalaxb.Fault[%s]]".format(op.name, arg(input), faultsToTypeName(faults))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s).transform({ case (header, body) => 
          |            %s }, {
          |""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString,
            outputString(output, binding, op, soapBindingStyle, soap12)) +
          // TODO: handle multiple faults
       """|              case x: %s => %s
          |""".stripMargin.format(faultTypeName, faultString(faults)) +
       """|              case x => x
          |            })""".stripMargin

      case (DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)), false) =>
        // "def %s(%s): Option[scalaxb.Fault[%s]]".format(op.name, arg(input), faultsToTypeName(faults))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right((header, body)) =>
          |            Right(%s)
          |        }""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString,
            faultString(faults), outputString(output, binding, op, soapBindingStyle, soap12))

      case (DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)), true) =>
        // "def %s(%s): Either[scalaxb.Fault[Any], %s]".format(op.name, arg(input), paramTypeName)
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s).transform({ case (header, body) => 
          |            %s }, {
          |""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString,
            outputString(output, binding, op, soapBindingStyle, soap12)) +
          // TODO: handle multiple faults
       """|              case x: %s => %s
          |""".stripMargin.format(faultTypeName, faultString(faults)) +
       """|              case x => x
          |            })""".stripMargin

      case (DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)), false) =>
        // "def %s(%s): Either[scalaxb.Fault[Any], %s]".format(op.name, arg(input), paramTypeName)
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right((header, body)) =>
          |            Right(%s)
          |        }""".stripMargin.format(
            bodyString(op, input, binding, soapBindingStyle),
            headerString(op, input, binding, soapBindingStyle), address, quotedMethod, actionString,
            faultString(faults), outputString(output, binding, op, soapBindingStyle, soap12))

      case (DataRecord(_, _, XNotificationoperationSequence(output)), true) =>
        // "def %s: %s".format(op.name, paramTypeName)
        """soapClient.requestResponse(Nil, defaultScope, %s, %s, %s).map({ case (header, body)) =>
          |            %s
          |        })""".stripMargin.format(address, quotedMethod, actionString, outputString(output, binding, op, soapBindingStyle, soap12))

      case (DataRecord(_, _, XNotificationoperationSequence(output)), false) =>
        // "def %s: %s".format(op.name, paramTypeName)
        """soapClient.requestResponse(Nil, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => sys.error(x.toString)
          |          case Right((header, body)) =>
          |            %s
          |        }""".stripMargin.format(address, quotedMethod, actionString, outputString(output, binding, op, soapBindingStyle, soap12))
      case _ => sys.error("unsupported.")
    }

    val retval = makeOperation(binding, intf, defaultSoapBindingStyle, soap12) + " = " + NL +
      "        " + opImpl
    logger.debug(retval)
    retval
  }

  case class BodyBinding(encodingStyle: Option[String], namespace: Option[String])

  // http://www.w3.org/TR/wsdl#_soap:body
  def bodyBinding(spec: Option[XStartWithExtensionsTypable]): BodyBinding = {
    val b = spec flatMap {
      _.any collect {
        case DataRecord(_, Some("body"), node: Node) => node
      } headOption
    }
    val literal = 
      b flatMap { node =>
        (node \ "@use").headOption map {_.text == "literal"}
      } getOrElse {true}
    if (!literal) {
      sys.error(encodedErrorMessage)
    } // if    
    BodyBinding(
      b flatMap { node => (node \ "@encodingStyle").headOption map {_.text} },
      b flatMap { node => (node \ "@namespace").headOption map {_.text} }
    )
  }

  case class HeaderBinding(message: javax.xml.namespace.QName, part: String,
                           encodingStyle: Option[String], namespace: Option[String])

  // http://www.w3.org/TR/wsdl#_soap:header
  def headerBindings(spec: Option[XStartWithExtensionsTypable]): Seq[HeaderBinding] = {
    val b: Seq[Node] = spec.toSeq flatMap {
      _.any collect {
        case DataRecord(_, Some("header"), node: Node) => node
      }
    }
    b map { node =>
      val literal = (node \ "@use").headOption map {_.text == "literal"} getOrElse {true}
      if (!literal) {
        sys.error(encodedErrorMessage)
      } // if
      HeaderBinding(masked.scalaxb.fromXML[javax.xml.namespace.QName](node \ "@message")(masked.scalaxb.XMLStandardTypes.qnameXMLFormat(node.scope)),
        (node \ "@part").text,
        (node \ "@encodingStyle").headOption map {_.text},
        (node \ "@namespace").headOption map {_.text}
      )
    }
  }

  def headerString(op: XOperationType, input: XParamType, binding: XBinding_operationType, soapBindingStyle: SoapBindingStyle): String =
    headerBindings(binding.input).toList flatMap { b =>
      val message = context.messages(splitTypeName(b.message))
      message.part find {_.name == Some(b.part)} map { p =>
        val param = toParamCache(p)
        val v = param.toParamName
        val label =
          if (p.element.isDefined) "\"%s\"".format(toElement(p).name)
          else "\"%s\"".format(p.name.getOrElse {"in"})
        val namespace =
          if (p.element.isDefined) toElement(p).namespace
          else if (soapBindingStyle == DocumentStyle) None
          else b.namespace
        val nsString = namespace map {"Some(\"%s\")".format(_)} getOrElse {"None"}
        val post =
          if ((soapBindingStyle == DocumentStyle) && !p.element.isDefined) """ match {
  case e: scala.xml.Elem => e.child
  case _ => sys.error("Elem not found!")
}"""
          else ""
        "scalaxb.toXML(%s, %s, %s, defaultScope)%s".format(v, nsString, label, post)
      }
    } match {
      case Nil => "Nil"
      case x :: Nil => x
      case xs => "Seq.concat(%s)" format (xs.mkString("," + NL + "              "))
    }

  // http://www.w3.org/TR/wsdl#_soap:body
  // "If use is literal, then each part references a concrete schema definition using either the element or type attribute."
  // http://www.w3.org/TR/soap12-part0/#L1185
  def bodyString(op: XOperationType, input: XParamType, binding: XBinding_operationType, soapBindingStyle: SoapBindingStyle): String = {
    val b = bodyBinding(binding.input)
    val (headerParts, bodyParts) = splitParamToParts(input, binding.input)
    // called only for DocumentStyle
    def entity(part: XPartType) = toParamCache(part).typeSymbol match {
      case AnyType(_) => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case symbol: BuiltInSimpleTypeSymbol => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case _ =>
        "%s(%s)".format(toParamCache(bodyParts.head).baseTypeName, buildIRIStyleArgs(input) map {_.toVarg} mkString(", "))
    }

    lazy val opLabel = "\"%s\"".format(op.name)
    lazy val prefix = "targetNamespace map {defaultScope.getPrefix(_)} getOrElse {\"\"}"

    lazy val args = bodyParts map { p =>
      val v = escapeKeyWord(soapBindingStyle match {
        case DocumentStyle => if (isMultiPart(input, binding.input)) toParamCache(p).toParamName
                              else entity(p)
        case RpcStyle      => p.name.getOrElse {"in"}
      })
      val (label, namespace) = soapBindingStyle match {
        case RpcStyle if p.element.isDefined =>
          ("\"%s\"".format(toElement(p).name), toElement(p).namespace)
        case RpcStyle =>
          ("\"%s\"".format(p.name.getOrElse {"in"}), b.namespace)        
        // If the operation style is document there are no additional wrappers, and the message parts appear directly under the SOAP Body element.
        case DocumentStyle if p.element.isDefined =>
          ("\"%s\"".format(toElement(p).name), toElement(p).namespace)
        case DocumentStyle =>
          ("\"Body\"", None)
      }

      val nsString = namespace map {"Some(\"%s\")".format(_)} getOrElse {"None"}
      
      "scalaxb.toXML(%s, %s, %s, defaultScope)".format(v, nsString, label) +
      (soapBindingStyle match {
        case DocumentStyle if !p.element.isDefined =>
          """ match {
      }
  case e: scala.xml.Elem => e.child
  case _ => sys.error("Elem not found!")
}"""
        case _ => ""
      })
    }
    lazy val argsString =
      args.headOption map { _ => args.mkString("  ++ " + NL + "          ") } getOrElse {"Nil"}
    (soapBindingStyle, args) match {
      case (DocumentStyle, x :: xs) => x
      case (DocumentStyle, _)       => "Nil"
      case _ =>
        """scala.xml.Elem(%s, %s, scala.xml.Null, defaultScope, true,
          %s: _*)""".format(prefix, opLabel, argsString)
    }
  }

  // body contains Seq[Node] collected by running 
  // Right(header, soapResponse.Body.any collect {
  //   case DataRecord(_, _, x: scala.xml.Node) => x
  // })
  // http://www.ibm.com/developerworks/library/ws-whichwsdl/
  def outputString(output: XParamType, binding: XBinding_operationType,
                   op: XOperationType, soapBindingStyle: SoapBindingStyle, soap12: Boolean): String = {
    val parts = paramMessage(output).part
    if (parts.isEmpty) "()"
    else {
      val b = bodyBinding(binding.output)
      val multipart = isMultiPart(output, binding.output)
      val (headerParts, bodyParts) = splitParamToParts(output, binding.output)
      val fromXmls = (bodyParts map { p =>
        val v = (soapBindingStyle, p.element) match {
          // If the operation style is document there are no additional wrappers, and the message parts appear directly under the SOAP Body element.
          // <soap:body>
          //   <xElement>5</xElement>
          //   <yElement>5.0</yElement>
          // </soap:body>
          case (DocumentStyle, Some(elementQName)) =>
            val elem = xsdgenerator.elements(splitTypeName(elementQName))
            """({
              scala.xml.Elem(null, "Body", scala.xml.Null, defaultScope, true, body.toSeq: _*)
            } \ "%s").head""" format (elem.name)
          case (DocumentStyle, None) =>
            """body.head"""
          case (RpcStyle, Some(elementQName)) =>
            val elem = xsdgenerator.elements(splitTypeName(elementQName))
            """(body.head \ "%s").head""" format (elem.name)
          case (RpcStyle, None) =>
            """(body.head \ "%s").head""" format (p.name.get)
        }
        buildPartArg(p, v) + (soapBindingStyle match {
          case DocumentStyle =>
            singleOutputType(output, soapBindingStyle) map { elem =>
              val param = xsdgenerator.buildParam(elem)
              "." + param.toParamName
            } getOrElse {""}
          case _ => ""
        })
      }) ++ (headerParts map { p =>
        val v =
          if (p.element.isDefined) """(<x>{header}</x> \ "%s").head""" format (p.element.get.getLocalPart)
          else """(<x>{header}</x> \ "%s").head""" format (p.name.get)
        buildPartArg(p, v)
      })

      if (!multipart) fromXmls.head
      else "%s(%s)" format (xsdgenerator.buildFullyQualifiedNameFromPackage(pkg, makeOperationOutputWrapperName(op)),
        fromXmls.mkString("," + NL + "              "))
    }
  }

  def buildPartArg(part: XPartType, selector: String): String =
    (part.typeValue, part.element) match {
      case (Some(typeValueQName), _) =>
        val typeSymbol = toTypeSymbol(typeValueQName)
        xsdgenerator.buildArg(xsdgenerator.buildTypeName(typeSymbol), selector, Single, None)
      case (_, Some(elementQName)) =>
        val elem = xsdgenerator.elements(splitTypeName(elementQName))
        xsdgenerator.buildArg(elem, selector, None, false)
      case _  => sys.error("part does not have either type or element: " + part.toString)
    }

  def paramMessage(input: XParamType): XMessageType = context.messages(splitTypeName(input.message))

  def escapeKeyWord(name: String) = if(scalaNames.isKeyword(name)) s"`$name`" else name

  case class ParamCache(paramName: String, typeSymbol: XsTypeSymbol, cardinality: Cardinality, nillable: Boolean, seqParam: Boolean) {
    def singleTypeName: String =
      if (nillable) "Option[" + baseTypeName + "]"
      else baseTypeName      
    def typeName: String =
      cardinality match {
        case Single   => singleTypeName
        case Optional => "Option[" + singleTypeName + "]"
        case Multiple => "Seq[" + singleTypeName + "]"
      }
    def baseTypeName: String = xsdgenerator.buildTypeName(typeSymbol)
    def toParamName = escapeKeyWord(paramName)
    def toScalaCode: String = "%s: %s" format(toParamName, typeName)
    def toVarg: String =
      if (seqParam) toParamName + ": _*"
      else toParamName
  }

  def buildRPCStyleArg(part: XPartType): ParamCache = toParamCache(part)

  def buildRPCStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.toList map {buildRPCStyleArg}

  def buildIRIStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.headOption map { part =>
    val paramName = part.name getOrElse {"in"}
    val pc = toParamCache(part)
    pc.typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol =>
        List(pc)
      case symbol@ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
        List(pc)
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        import scalaxb.compiler.xsd.{Multiple, AllDecl, ComplexContentDecl, CompContRestrictionDecl, CompContExtensionDecl}
        val flatParticles = xsdgenerator.flattenElements(decl)
        val attributes = xsdgenerator.flattenAttributes(decl)
        val list = List.concat(flatParticles, attributes)
        val primary = decl.content match {
          case ComplexContentDecl(CompContRestrictionDecl(_, x, _)) => x
          case ComplexContentDecl(CompContExtensionDecl(_, x, _)) => x
          case _ => None
        }
        val longAll: Boolean = primary match {
          case Some(all: AllDecl) if  xsdgenerator.isLongAll(all, decl.namespace, decl.family) => true
          case _ => false
        }

        list map { x =>
          val param = xsdgenerator.buildParam(x) map {camelCase}
          val seqParam = (list.size == 1) && (param.cardinality == Multiple) &
            (attributes.size == 0) && (!decl.mixed) && (!longAll)
          ParamCache(param.toParamName, param.typeSymbol, param.cardinality, param.nillable, seqParam)
        }
      case AnyType(symbol) =>
        List(pc)
      case x => sys.error("unexpected type: " + x)
    }
  } getOrElse {sys.error("unexpected input: " + input)}

  def buildPartsArg(input: XParamType): String = (paramMessage(input).part map { part =>
    "%s: %s".format(part.name getOrElse {"in"}, toParamCache(part).typeName)
  }).mkString(", ")

  def toParamCache(part: XPartType): ParamCache =
    part.typeValue map { typeValue =>
      val name = camelCase(part.name getOrElse "in")
      ParamCache(name, toTypeSymbol(typeValue), Single, false, false)
    } getOrElse {
      part.element map { element =>
        val param = xsdgenerator.buildParam(xsdgenerator.elements(splitTypeName(element))) map {camelCase}
        ParamCache(param.toParamName, param.typeSymbol, param.cardinality, param.nillable, false)
      } getOrElse {sys.error("part does not have either type or element: " + part.toString)}
    }

  def toTypeSymbol(qname: javax.xml.namespace.QName): XsTypeSymbol = {
    import scalaxb.compiler.xsd.{TypeSymbolParser, ReferenceTypeSymbol}
    val symbol = TypeSymbolParser.fromQName(qname)
    symbol match {
      case symbol: ReferenceTypeSymbol =>
        val (namespace, typeName) = splitTypeName(qname)
        symbol.decl = xsdgenerator.getTypeGlobally(namespace, typeName, context.xsdcontext)
      case _ =>
    }
    symbol
  }

  def toElement(part: XPartType) = part.element map  { element =>
    xsdgenerator.elements(splitTypeName(element))
  } getOrElse {sys.error("part does not have an element: " + part.toString)}

  def singleOutputPart(output: XParamType): Option[XPartType] =
    paramMessage(output).part.headOption

  def singleOutputType(output: XParamType, soapBindingStyle: SoapBindingStyle): Option[scalaxb.compiler.xsd.ElemDecl] =
    soapBindingStyle match {
      case DocumentStyle =>
        paramMessage(output).part.headOption map { part =>
          import scalaxb.compiler.xsd.{ReferenceTypeSymbol, ComplexTypeDecl, Single}
          toParamCache(part).typeSymbol match {
            case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
              val flatParticles = xsdgenerator.flattenElements(decl)
              val attributes = xsdgenerator.flattenAttributes(decl)
              if (decl.mixed) None
              else if (flatParticles.size == 1 && attributes.size == 0) {
                val head = flatParticles.head
                if (xsdgenerator.buildParam(head).cardinality == Single) Some(head)
                else None
              }
              else None
            case x => None
          }
        } getOrElse {None}
      case RpcStyle => None
    }

  def faultsToTypeName(faults: Seq[XFaultType], soap12: Boolean): String =
    "%s[%s]" format (if (soap12) "scalaxb.Fault"
      else "scalaxb.Soap11Fault",
    faultsToFaultParamTypeName(faults) match {
      case (x, true) => s"""Option[$x]"""
      case (x, _)    => x
    })
  // param type and nillable
  def faultsToFaultParamTypeName(faults: Seq[XFaultType]): (String, Boolean) =
    faults.toList match {
      case x :: xs =>
        val msg = context.messages(splitTypeName(x.message))
        msg.part.headOption map { part =>
          val pc = toParamCache(part)
          (pc.baseTypeName, pc.nillable)
        } getOrElse ("Any", false)
      case _ => ("Any", false)
    }

  def makeBindingName(binding: XBindingType): String = {
    val name = xsdgenerator.makeTypeName(binding.name)
    if (name.endsWith("Binding")) name
    else name + "Binding"
  }

  def findPort(binding: XBindingType) =
    for {
      service <- context.services.valuesIterator.toList
      port <- service.port if binding == context.bindings(splitTypeName(port.binding))
    } yield port

  def elements(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     => throw new ReferenceNotFound("element" , namespace, name)
      }

  def splitTypeName(qname: javax.xml.namespace.QName) = (Option[String](qname.getNamespaceURI), qname.getLocalPart)

  implicit def boolToOption(b: Boolean): Option[Unit] = if (b) Some(()) else None
}
