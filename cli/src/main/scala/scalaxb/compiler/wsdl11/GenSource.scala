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
  import wsdl11._
  import scalaxb.{DataRecord}
  import scalaxb.compiler.{Config, Snippet, ReferenceNotFound, Module}
  import Module.{NL, indent, camelCase}
  import scala.xml.Node
  import scalaxb.compiler.xsd.{ReferenceTypeSymbol, SimpleTypeDecl, ComplexTypeDecl, BuiltInSimpleTypeSymbol,
    XsTypeSymbol, AnyType, XsAnyType}
  import com.weiglewilczek.slf4s.Logger

  val WSDL_SOAP11 = "http://schemas.xmlsoap.org/wsdl/soap/"
  val WSDL_SOAP12 = "http://schemas.xmlsoap.org/wsdl/soap12/"
  val WSDL_HTTP = "http://schemas.xmlsoap.org/wsdl/http"
  val SOAP_MEP_REQUEST_RESPONSE = "http://www.w3.org/2003/05/soap/mep/request-response"
  val SOAP_MEP_SOAP_RESPONSE = "http://www.w3.org/2003/05/soap/mep/soap-response"

  lazy val logger = Logger("wsdl.GenSource")
  def context: WsdlContext
  def scope: scala.xml.NamespaceBinding
  def schemas = context.xsdcontext.schemas.toList
  def xsdgenerator: scalaxb.compiler.xsd.GenSource

  def generate(definition: XDefinitionsType): Snippet = {
    logger.debug("generate")
    Snippet(
      (soap11Bindings(definition) map {makeSoap11Binding}) ++
      (soap12Bindings(definition) map {makeSoap12Binding}): _*)
  }

  def soap12Bindings(definition: XDefinitionsType) =
    definition.binding filter { binding =>
      binding.any exists {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP12
        case _ => false
      }
    }

  def soap11Bindings(definition: XDefinitionsType) =
    if (!soap12Bindings(definition).isEmpty) Nil
    else definition.binding filter { binding =>
      binding.any exists {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP11
        case _ => false
      }
    }

  def isMultiPart(param: XParamType, bindingOption: Option[XStartWithExtensionsTypable]): Boolean =
    (paramMessage(param).part.size > 1) ||
    (!headerBindings(bindingOption).isEmpty)

  def isEmptyPart(param: XParamType, bindingOption: Option[XStartWithExtensionsTypable]): Boolean =
    paramMessage(param).part.isEmpty && headerBindings(bindingOption).isEmpty

  // generate method signature
  def makeOperation(binding: XBinding_operationType, intf: XPortTypeType, defaultDocument: Boolean): String = {
    val op = boundOperation(binding, intf)
    val document = isDocument(binding, defaultDocument)

    def arg(input: XParamType): String =
      if (document) {
        if (isEmptyPart(input, binding.input)) ""
        else if (!isMultiPart(input, binding.input)) buildIRIStyleArgs(input) map {_.toScalaCode} mkString(", ")
        else makeOperationInputArgs(binding, intf) map {_.toScalaCode} mkString(", ")
      }
      else buildRPCStyleArgs(input) map {_.toScalaCode} mkString(", ")

    val name = camelCase(op.name)
    logger.debug("makeOperation: " + name)

    val retval = op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) =>
        "def %s(%s): Unit".format(name, arg(input))
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(binding, op, output, document))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(binding, op, output, document))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        "def %s: %s".format(name, outputTypeName(binding, op, output, document))
      case _ => error("unsupported.")
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
      case _ => error("unsupported.")
    }

  def makeOperationOutputWrapperName(op: XOperationType): String =
    xsdgenerator.makeTypeName(op.name + "Output")

  def makeOperationWrapperParams(op: XOperationType, headers: Seq[HeaderBinding],
                                 symbol: XsTypeSymbol): Seq[ParamCache] = {
    val param = ParamCache("value", xsdgenerator.buildTypeName(symbol), false)
    val headerParams = headers flatMap { header =>
      val message = context.messages(splitTypeName(header.message))
      message.part find {_.name == Some(header.part)} map {toParamCache}
    }
    param +: headerParams
  }

  def makeOperationInputArgs(binding: XBinding_operationType, intf: XPortTypeType): Seq[ParamCache] = {
    val op = boundOperation(binding, intf)
    val headers = headerBindings(binding.input)
    val symbol =  operationParts(op) match {
      case (Some(input), _, _) => toTypeSymbol(input)
      case _ => error("expected input:" + op.name)
    }
    makeOperationWrapperParams(op, headers, symbol)
  }

  def makeOperationOutputArgs(binding: XBinding_operationType, intf: XPortTypeType): Seq[ParamCache] = {
    val op = boundOperation(binding, intf)
    val headers = headerBindings(binding.output)
    val symbol =  operationParts(op) match {
      case (_, Some(ouput), _) => toTypeSymbol(ouput)
      case _ => error("expected ouput: " + op.name)
    }
    makeOperationWrapperParams(op, headers, symbol)
  }

  // generate case class for op binding in case the soap header is bound.
  def makeOperationOutput(binding: XBinding_operationType, intf: XPortTypeType): Option[String] = {
    val op = boundOperation(binding, intf)
    operationParts(op)._2 flatMap { output =>
      isMultiPart(output, binding.output) map { _ =>
        val op = boundOperation(binding, intf)
        "case class %s(%s)" format(
          makeOperationOutputWrapperName(op),
          makeOperationOutputArgs(binding, intf) map {_.toScalaCode} mkString(", ")
        )
      }
    }
  }

  def boundOperation(binding: XBinding_operationType, intf: XPortTypeType) =
    (intf.operation filter {_.name == binding.name}).headOption getOrElse {
      error("operation %s was not found in %s".format(binding.name, intf.name))
    }

  def isDocument(binding: XBinding_operationType, defaultDocument: Boolean) =
    binding.any.headOption match {
      case Some(DataRecord(_, _, node: Node)) => (node \ "@style").headOption map {
        _.text == "document"} getOrElse {defaultDocument}
      case _ => defaultDocument
    }

  // generate method impl
  def makeSoap12OpBinding(binding: XBinding_operationType, intf: XPortTypeType,
                           defaultDocument: Boolean): String = {
    val op = boundOperation(binding, intf)
    logger.debug("makeSoap12OpBinding: " + op.name)

    val address = "baseAddress"
    val quotedMethod = "\"POST\""
    val action = binding.any.headOption match {
      case Some(DataRecord(_, _, node: Node)) => (node \ "@soapAction").headOption map {_.text}
      case _ => None
    }
    val document = isDocument(binding, defaultDocument)
    val actionString = action map {"Some(new java.net.URI(\"%s\"))".format(_)} getOrElse {"None"}

    def faultString(faults: Seq[XFaultType]): String = faultsToTypeName(faults) match {
      case "Any" => "x"
      case x     => "x.asFault[%s]".format(x)
    }

    val opImpl = op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) =>
        // "def %s(%s): Unit".format(op.name, arg(input))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x.toString)
          |          case Right(x) => ()
          |        }""".stripMargin.format(bodyString(op, input, binding, document),
            headerString(op, input, binding, document), address, quotedMethod, actionString)
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        // "def %s(%s): Option[scalaxb.Fault[%s]]".format(op.name, arg(input), faultsToTypeName(faults))
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right((header, body)) =>
          |            Right(%s)
          |        }""".stripMargin.format(
            bodyString(op, input, binding, document),
            headerString(op, input, binding, document), address, quotedMethod, actionString,
            faultString(faults), outputString(output, binding, op, document))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        // "def %s(%s): Either[scalaxb.Fault[Any], %s]".format(op.name, arg(input), paramTypeName)
        """soapClient.requestResponse(%s,
          |            %s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right((header, body)) =>
          |            Right(%s)
          |        }""".format(
            bodyString(op, input, binding, document),
            headerString(op, input, binding, document), address, quotedMethod, actionString,
            faultString(faults), outputString(output, binding, op, document))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        // "def %s: %s".format(op.name, paramTypeName)
        """soapClient.requestResponse(Nil, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x.toString)
          |          case Right((header, body)) =>
          |            %s
          |        }""".stripMargin.format(address, quotedMethod, actionString, outputString(output, binding, op, document))
      case _ => error("unsupported.")
    }

    val retval = makeOperation(binding, intf, defaultDocument) + " = " + NL +
      "        " + opImpl
    logger.debug(retval)
    retval
  }

  case class BodyBinding(literal: Boolean, encodingStyle: Option[String], namespace: Option[String])

  // http://www.w3.org/TR/wsdl#_soap:body
  def bodyBinding(spec: Option[XStartWithExtensionsTypable]): BodyBinding = {
    val b = spec flatMap {
      _.any collect {
        case DataRecord(_, Some("body"), node: Node) => node
      } headOption
    }
    BodyBinding(b flatMap { node =>
        (node \ "@use").headOption map {_.text == "literal"}
      } getOrElse {true},
      b flatMap { node => (node \ "@encodingStyle").headOption map {_.text} },
      b flatMap { node => (node \ "@namespace").headOption map {_.text} }
    )
  }

  case class HeaderBinding(literal: Boolean, message: javax.xml.namespace.QName, part: String,
                           encodingStyle: Option[String], namespace: Option[String])

  // http://www.w3.org/TR/wsdl#_soap:header
  def headerBindings(spec: Option[XStartWithExtensionsTypable]): Seq[HeaderBinding] = {
    val b: Seq[Node] = spec.toSeq flatMap {
      _.any collect {
        case DataRecord(_, Some("header"), node: Node) => node
      }
    }
    b map { node =>
      HeaderBinding((node \ "@use").headOption map {_.text == "literal"} getOrElse {true},
        scalaxb.fromXML[javax.xml.namespace.QName](node \ "@message")(scalaxb.qnameXMLFormat(node.scope)),
        (node \ "@part").text,
        (node \ "@encodingStyle").headOption map {_.text},
        (node \ "@namespace").headOption map {_.text}
      )
    }
  }

  def headerString(op: XOperationType, input: XParamType, binding: XBinding_operationType, document: Boolean): String =
    headerBindings(binding.input).toList flatMap { b =>
      val message = context.messages(splitTypeName(b.message))
      message.part find {_.name == Some(b.part)} map { p =>
        val param = toParamCache(p)
        val v = param.toParamName
        val label =
          if (b.literal && p.element.isDefined) "\"%s\"".format(toElement(p).name)
          else "\"%s\"".format(p.name.getOrElse {"in"})
        val namespace =
          if (b.literal && p.element.isDefined) toElement(p).namespace
          else if (b.literal && document) None
          else b.namespace
        val nsString = namespace map {"Some(\"%s\")".format(_)} getOrElse {"None"}
        val post =
          if (b.literal && document && !p.element.isDefined) """ match {
  case e: scala.xml.Elem => e.child
  case _ => error("Elem not found!")
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
  def bodyString(op: XOperationType, input: XParamType, binding: XBinding_operationType, document: Boolean): String = {
    val b = bodyBinding(binding.input)
    lazy val entity = toTypeSymbol(input) match {
      case AnyType(_) => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case symbol: BuiltInSimpleTypeSymbol => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) => (buildIRIStyleArgs(input) map {_.toParamName}).head
      case _ =>
        "%s(%s)".format(paramTypeName(input), buildIRIStyleArgs(input) map {_.toVarg} mkString(", "))
    }

    lazy val opLabel = "\"%s\"".format(op.name)
    lazy val prefix = "targetNamespace map {defaultScope.getPrefix(_)} getOrElse {\"\"}"

    lazy val args = paramMessage(input).part map { p =>
      val v =
        if (document) {
          if (isMultiPart(input, binding.input)) "value"
          else entity
        }
        else p.name.getOrElse {"in"}
      val label =
        if (b.literal && p.element.isDefined) "\"%s\"".format(toElement(p).name)
        else if (b.literal && document) """"Body""""
        else "\"%s\"".format(p.name.getOrElse {"in"})
      val namespace =
        if (b.literal && p.element.isDefined) toElement(p).namespace
        else if (b.literal && document) None
        else b.namespace
      val nsString = namespace map {"Some(\"%s\")".format(_)} getOrElse {"None"}
      val post =
        if (b.literal && document && !p.element.isDefined) """ match {
  case e: scala.xml.Elem => e.child
  case _ => error("Elem not found!")
}"""
        else ""

      "scalaxb.toXML(%s, %s, %s, defaultScope)%s".format(v, nsString, label, post)
    }
    lazy val argsString =
      args.headOption map { _ => args.mkString("  ++ " + NL + "          ") } getOrElse {"Nil"}

    if (document) args match {
      case x :: xs => x
      case _ => "Nil"
    }
    else """scala.xml.Elem(%s, %s, scala.xml.Null, defaultScope,
          %s: _*)""".format(prefix, opLabel, argsString)
  }

  def outputString(output: XParamType, binding: XBinding_operationType,
                   op: XOperationType, document: Boolean): String = {
    val parts = paramMessage(output).part
    if (parts.isEmpty) "()"
    else {
      val b = bodyBinding(binding.output)
      val multipart = isMultiPart(output, binding.output)
      val fromXmls = (parts map { p =>
        val v =
          if (b.literal && p.element.isDefined) "body.headOption getOrElse {body}"
          else if (b.literal) """scala.xml.Elem("", "Body", scala.xml.Null, defaultScope, body.toSeq: _*)"""
          else """(body.head \ "%s").head""" format (p.name.get)

        val post =
          if (document) singleOutputType(output, document) map { elem =>
            val param = xsdgenerator.buildParam(elem)
            "." + param.toParamName
          } getOrElse {""}
          else ""

        "scalaxb.fromXML[%s](%s)%s".format(partTypeName(p), v, post)
      }) ++ (headerBindings(binding.output) flatMap { b =>
        val message = context.messages(splitTypeName(b.message))
        message.part find {_.name == Some(b.part)} map { p =>
          val v =
            if (b.literal && p.element.isDefined) """(header \ "%s").head""" format (p.element.get.getLocalPart)
            else """(header \ "%s").head""" format (p.name.get)
          "scalaxb.fromXML[%s](%s)".format(partTypeName(p), v)
        }
      })

      if (!multipart) fromXmls.head
      else "%s(%s)" format (makeOperationOutputWrapperName(op), fromXmls.mkString("," + NL + "              "))
    }
  }

  def paramMessage(input: XParamType): XMessageType = context.messages(splitTypeName(input.message))

  case class ParamCache(toParamName: String, typeName: String, seqParam: Boolean) {
    def toScalaCode: String = "%s: %s" format(toParamName, typeName)
    def toVarg: String =
      if (seqParam) toParamName + ": _*"
      else toParamName
  }

  def buildRPCStyleArg(part: XPartType): ParamCache =
    ParamCache(part.name getOrElse {"in"}, xsdgenerator.buildTypeName(toTypeSymbol(part)), false)

  def buildRPCStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.toList map {buildRPCStyleArg}

  def buildIRIStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.headOption map { part =>
    val paramName = part.name getOrElse {"in"}
    toTypeSymbol(part) match {
      case symbol: BuiltInSimpleTypeSymbol =>
        List(ParamCache(paramName, xsdgenerator.buildTypeName(symbol), false))
      case symbol@ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
        List(ParamCache(paramName, xsdgenerator.buildTypeName(symbol), false))
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        import scalaxb.compiler.xsd.{Multiple, AllDecl, ComplexContentDecl, CompContRestrictionDecl, CompContExtensionDecl}
        val flatParticles = xsdgenerator.flattenElements(decl, 0)
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
          ParamCache(param.toParamName, param.typeName, seqParam)
        }
      case AnyType(symbol) =>
        List(ParamCache(paramName, xsdgenerator.buildTypeName(symbol), false))
      case x => error("unexpected type: " + x)
    }
  } getOrElse {error("unexpected input: " + input)}

  def buildPartsArg(input: XParamType): String = (paramMessage(input).part map { part =>
    "%s: %s".format(part.name getOrElse {"in"}, partTypeName(part))
  }).mkString(", ")

  def paramTypeName(param: XParamType): String = xsdgenerator.buildTypeName(toTypeSymbol(param), true)

  def partTypeName(part: XPartType): String = xsdgenerator.buildTypeName(toTypeSymbol(part), true)

  def toTypeSymbol(param: XParamType): XsTypeSymbol =
    paramMessage(param).part.headOption map { toTypeSymbol(_) } getOrElse {XsAnyType}

  def toParamCache(part: XPartType): ParamCache =
    part.typeValue map { typeValue =>
      val name = camelCase(part.name.get)
      ParamCache(name, xsdgenerator.buildTypeName(toTypeSymbol(typeValue)), false)
    } getOrElse {
      part.element map { element =>
        val param = xsdgenerator.buildParam(xsdgenerator.elements(splitTypeName(element))) map {camelCase}
        ParamCache(param.toParamName, param.typeName, false)
      } getOrElse {error("part does not have either type or element: " + part.toString)}
    }

  def toTypeSymbol(part: XPartType): XsTypeSymbol =
    part.typeValue map { toTypeSymbol(_) } getOrElse {
      part.element map { element => xsdgenerator.elements(splitTypeName(element)).typeSymbol
      } getOrElse {error("part does not have either type or element: " + part.toString)}
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
  } getOrElse {error("part does not have an element: " + part.toString)}

  def outputTypeName(binding: XBinding_operationType, op: XOperationType,
                     output: XParamType, document: Boolean): String =
    if (headerBindings(binding.output).isEmpty) {
      singleOutputType(output, document) map { elem =>
        val param = xsdgenerator.buildParam(elem)
        param.typeName
      } getOrElse {
        if (paramMessage(output).part.isEmpty) "Unit"
        else paramTypeName(output)
      }}
    else makeOperationOutputWrapperName(op)

  def singleOutputPart(output: XParamType): Option[XPartType] =
    paramMessage(output).part.headOption

  def singleOutputType(output: XParamType, document: Boolean): Option[scalaxb.compiler.xsd.ElemDecl] =
    if (document) paramMessage(output).part.headOption map { part =>
      import scalaxb.compiler.xsd.{ReferenceTypeSymbol, ComplexTypeDecl}
      toTypeSymbol(part) match {
        case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
          val flatParticles = xsdgenerator.flattenElements(decl, 0)
          val attributes = xsdgenerator.flattenAttributes(decl)
          if (flatParticles.size == 1 && attributes.size == 0) Some(flatParticles.head)
          else None
        case x => None
      }
    } getOrElse {None}
    else None

  def faultsToTypeName(faults: Seq[XFaultType]): String = faults.toList match {
    case x :: xs =>
      val msg = context.messages(splitTypeName(x.message))
      msg.part.headOption map { part =>
        val symbol = toTypeSymbol(part)
        xsdgenerator.buildTypeName(symbol, true)
      } getOrElse {"Any"}
    case _ => "Any"
  }

  def makeBindingName(binding: XBindingType): String = {
    val name = xsdgenerator.makeTypeName(binding.name)
    if (name.endsWith("Binding")) name
    else name + "Binding"
  }

  def makeSoap11Binding(binding: XBindingType): Snippet = {
    val name = makeBindingName(binding)
    logger.debug("makeSoap11Binding: " + name)

    val interfaceType = context.interfaces(splitTypeName(binding.typeValue))
    val interfaceTypeName = interfaceType.name.capitalize
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP11 =>
        Some((node \ "@location").text)
      case _ => None
    }}

    val document = binding.any.headOption map {
      case DataRecord(_, _, node: Node) =>
        val style = (node \ "@style").text
        style == "" || style == "document"
    } getOrElse {true}

    val addressString = address map {"""lazy val baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val operationOutputs = binding.operation flatMap { makeOperationOutput(_, interfaceType) }
    val operations = binding.operation map { opBinding => makeOperation(opBinding, interfaceType, document) }
    val bindingOps = binding.operation map { opBinding => makeSoap12OpBinding(opBinding, interfaceType, document) }

    val interfaceTrait = <source>
trait {interfaceTypeName} {{
  {operations.mkString(NL + "  ")}
}}

{operationOutputs.mkString(NL + NL)}
</source>

    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.Soap11Clients =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(xsdgenerator.schema.targetNamespace) }
    lazy val service: {interfaceTypeName} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeName} {{
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
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP12 =>
        Some((node \ "@location").text)
      case _ => None
    }}

    val document = binding.any.headOption map {
      case DataRecord(_, _, node: Node) =>
        val style = (node \ "@style").text
        style == "" || style == "document"
    } getOrElse {true}

    val addressString = address map {"""lazy val baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val operations = binding.operation map { opBinding => makeOperation(opBinding, interfaceType, document) }
    val bindingOps = binding.operation map { opBinding => makeSoap12OpBinding(opBinding, interfaceType, document) }

    val interfaceTrait = <source>
trait {interfaceTypeName} {{
  {operations.mkString(NL + "  ")}
}}
</source>

    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.SoapClients =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(xsdgenerator.schema.targetNamespace) }
    lazy val service: {interfaceTypeName} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeName} {{
      {bindingOps.mkString(NL + "      ")}
    }}
  }}
</source>
    Snippet(interfaceTrait, <source/>, bindingTrait, <source/>)
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
