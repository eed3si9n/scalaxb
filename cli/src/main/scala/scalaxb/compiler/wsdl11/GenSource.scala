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

import scalaxb.compiler.{Logger, Config, Snippet, ReferenceNotFound, Module}
import Module.{NL, indent, camelCase}

trait GenSource {
  import wsdl11._
  import scalaxb.{DataRecord}
  import scala.collection.mutable
  import scala.xml.Node

  val WSDL_SOAP11 = "http://schemas.xmlsoap.org/wsdl/soap/"
  val WSDL_SOAP12 = "http://schemas.xmlsoap.org/wsdl/soap12/"
  val WSDL_HTTP = "http://schemas.xmlsoap.org/wsdl/http"
  val SOAP_MEP_REQUEST_RESPONSE = "http://www.w3.org/2003/05/soap/mep/request-response"
  val SOAP_MEP_SOAP_RESPONSE = "http://www.w3.org/2003/05/soap/mep/soap-response"

  def logger: Logger
  def log(msg: String) = logger.log(msg)
  def context: WsdlContext
  def scope: scala.xml.NamespaceBinding
  def schemas = context.xsdcontext.schemas.toList
  def xsdgenerator: scalaxb.compiler.xsd.GenSource

  def generate(definition: XDefinitionsType): Snippet = {
    log("wsdl11: GenSource.generate")
    Snippet(
      (soap11Bindings(definition) map {makeSoap11Binding}) ++
      (soap12Bindings(definition) map {makeSoap12Binding}): _*)
  }

  def soap12Bindings(definition: XDefinitionsType) =
    definition.binding filter { binding =>
      binding.any.headOption map {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP12
      } getOrElse {false}
    }

  def soap11Bindings(definition: XDefinitionsType) =
    if (!soap12Bindings(definition).isEmpty) Nil
    else definition.binding filter { binding =>
      binding.any.headOption map {
        case DataRecord(_, _, node: Node) => node.scope.getURI((node.prefix)) == WSDL_SOAP11
      } getOrElse {false}
    }

  // generate method signature
  def makeOperation(binding: XBinding_operationType, intf: XPortTypeType, defaultDocument: Boolean): String = {
    val op = boundOperation(binding, intf)
    val document = isDocument(binding, defaultDocument)

    def arg(input: XParamType): String =
      if (document) buildIRIStyleArgs(input) map {_.toScalaCode} mkString(", ")
      else buildRPCStyleArgs(input) map {_.toScalaCode} mkString(", ")

    val name = camelCase(op.name)
    log("wsdl11#makeOperation: " + name)

    val retval = op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) =>
        "def %s(%s): Unit".format(name, arg(input))
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(output, document))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(output, document))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        "def %s: %s".format(name, outputTypeName(output, document))
      case _ => error("unsupported.")
    }

    log("wsdl11#makeOperation: " + retval)
    retval
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
    log("wsdl11#makeSoap12OpBinding: " + op.name)

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
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x)
          |          case Right(x) => ()
          |        }""".stripMargin.format(invokeToXML(op, input, binding, document),
          address, quotedMethod, actionString)
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        // "def %s(%s): Option[scalaxb.Fault[%s]]".format(op.name, arg(input), faultsToTypeName(faults))
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right(x) => Right(%s)
          |        }""".stripMargin.format(
          invokeToXML(op, input, binding, document), address, quotedMethod, actionString,
          faultString(faults), outputString(output, binding, document))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        // "def %s(%s): Either[scalaxb.Fault[Any], %s]".format(op.name, arg(input), paramTypeName)
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right(x) => Right(%s)
          |        }""".format(
          invokeToXML(op, input, binding, document), address, quotedMethod, actionString,
          faultString(faults), outputString(output, binding, document))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        // "def %s: %s".format(op.name, paramTypeName)
        """soapClient.requestResponse(Nil, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x)
          |          case Right(x) => %s
          |        }""".stripMargin.format(address, quotedMethod, actionString, outputString(output, binding, document))
      case _ => error("unsupported.")
    }

    val retval = makeOperation(binding, intf, defaultDocument) + " = " + NL +
      "        " + opImpl
    log(retval)
    retval
  }

  case class BodyBinding(literal: Boolean, encodingStyle: Option[String], namespace: Option[String])

  // http://www.w3.org/TR/wsdl#_soap:body
  def bodyBinding(spec: Option[XStartWithExtensionsTypable]): BodyBinding = {
    val b = spec flatMap {
      _.any.headOption match {
        case Some(DataRecord(_, _, node: Node)) => Some(node)
        case _ => None
      }
    }
    BodyBinding(b flatMap { node =>
        (node \ "@use").headOption map {_.text == "literal"}
      } getOrElse {true},
      b flatMap { node => (node \ "@encodingStyle").headOption map {_.text} },
      b flatMap { node => (node \ "@namespace").headOption map {_.text} }
    )
  }

  // http://www.w3.org/TR/wsdl#_soap:body
  // "If use is literal, then each part references a concrete schema definition using either the element or type attribute."
  // http://www.w3.org/TR/soap12-part0/#L1185
  def invokeToXML(op: XOperationType, input: XParamType, binding: XBinding_operationType, document: Boolean): String = {
    val b = bodyBinding(binding.input)
    lazy val entity = "%s(%s)".format(paramTypeName(input), buildIRIStyleArgs(input) map {_.toParamName} mkString(", "))
    lazy val opLabel = "\"%s\"".format(op.name)
    lazy val prefix = "targetNamespace map {defaultScope.getPrefix(_)} getOrElse {\"\"}"

    lazy val args = paramMessage(input).part map { p =>
      val v =
        if (document) entity
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

    if (document) args.head
    else """scala.xml.Elem(%s, %s, scala.xml.Null, defaultScope,
          %s: _*)""".format(prefix, opLabel, args.mkString("  ++ " + NL + "          "))
  }

  def outputString(output: XParamType, binding: XBinding_operationType, document: Boolean): String = {
    val b = bodyBinding(binding.output)
    singleOutputPart(output) map { p =>
      val v =
        if (b.literal && p.element.isDefined) "x.head"
        else if (b.literal) """scala.xml.Elem("", "Body", scala.xml.Null, defaultScope, x.toSeq: _*)"""
        else """x.head \ "%s"""" format (p.name.get)

      val post =
        if (document) singleOutputType(output, document) map { elem =>
          val param = xsdgenerator.buildParam(elem)
          "." + param.toParamName
        } getOrElse {""}
        else ""

      "scalaxb.fromXML[%s](%s)%s".format(partTypeName(p), v, post)
    } getOrElse {"()"}
  }

  def paramMessage(input: XParamType): XMessageType = context.messages(splitTypeName(input.message.toString))

  def isOperationIRIStyleQualified(input: XParamType): Boolean = paramMessage(input).part.toList match {
    case part :: Nil =>
      import scalaxb.compiler.xsd.{ReferenceTypeSymbol, ComplexTypeDecl, BuiltInSimpleTypeSymbol}
      toTypeSymbol(part) match {
        case symbol: BuiltInSimpleTypeSymbol => true
        case ReferenceTypeSymbol(decl: ComplexTypeDecl) => xsdgenerator.isQualifyAsIRIStyle(decl)
        case _ => false
      }
    case _ => false
  }

  case class ParamCache(toParamName: String, toScalaCode: String)

  def buildRPCStyleArg(part: XPartType): ParamCache = {
    val paramName = part.name getOrElse {"in"}
    val scalaCode = "%s: %s".format(paramName, xsdgenerator.buildTypeName(toTypeSymbol(part)))
    ParamCache(paramName, scalaCode)
  }

  def buildRPCStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.toList map {buildRPCStyleArg}

  def buildIRIStyleArgs(input: XParamType): List[ParamCache] = paramMessage(input).part.headOption map { part =>
    import scalaxb.compiler.xsd.{ReferenceTypeSymbol, ComplexTypeDecl, BuiltInSimpleTypeSymbol}
    toTypeSymbol(part) match {
      case symbol: BuiltInSimpleTypeSymbol =>
        val paramName = part.name getOrElse {"in"}
        val scalaCode = "%s: %s".format(paramName, xsdgenerator.buildTypeName(symbol))
        List(ParamCache(paramName, scalaCode))
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        val flatParticles = xsdgenerator.flattenElements(decl, 0)
        flatParticles map { p =>
          val param = xsdgenerator.buildParam(p) map {camelCase}
          ParamCache(param.toParamName, param.toScalaCode)
        }
      case x => error("unexpected type: " + x)
    }
  } getOrElse {error("unexpected input: " + input)}

  def buildPartsArg(input: XParamType): String = (paramMessage(input).part map { part =>
    "%s: %s".format(part.name getOrElse {"in"}, partTypeName(part))
  }).mkString(", ")

  def partTypeName(part: XPartType): String = xsdgenerator.buildTypeName(toTypeSymbol(part), true)

  def toTypeSymbol(part: XPartType) = part.typeValue map { typeValue =>
    import scalaxb.compiler.xsd.{TypeSymbolParser, ReferenceTypeSymbol}
    val symbol = TypeSymbolParser.fromString(typeValue.toString, splitTypeName(typeValue.toString))
    symbol match {
      case symbol: ReferenceTypeSymbol =>
        val (namespace, typeName) = splitTypeName(typeValue.toString)
        symbol.decl = xsdgenerator.getTypeGlobally(namespace, typeName, context.xsdcontext)
      case _ =>
    }
    symbol
  } getOrElse {
    part.element map { element => xsdgenerator.elements(splitTypeName(element.toString)).typeSymbol
    } getOrElse {error("part does not have either type or element: " + part.toString)}
  }

  def toElement(part: XPartType) = part.element map  { element =>
    xsdgenerator.elements(splitTypeName(element.toString))
  } getOrElse {error("part does not have an element: " + part.toString)}

  def outputTypeName(output: XParamType, document: Boolean): String =
    singleOutputType(output, document) map { elem =>
      val param = xsdgenerator.buildParam(elem)
      param.typeName
    } getOrElse {paramTypeName(output)}

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

  def paramTypeName(param: XParamType): String =
    paramMessage(param).part.headOption map { part => partTypeName(part) } getOrElse {"Any"}

  def faultsToTypeName(faults: Seq[XFaultType]): String = faults.toList match {
    case x :: xs =>
      val msg = context.messages(splitTypeName(x.message.toString))
      msg.part.headOption map { part =>
        val symbol = toTypeSymbol(part)
        xsdgenerator.buildTypeName(symbol, true)
      } getOrElse {"Any"}
    case _ => "Any"
  }

  // @see http://www.w3.org/TR/2007/REC-wsdl20-adjuncts-20070626/#_http_operation_location_query_constr
  def buildInputArgs(input: XParamType): String = {
    import scalaxb.compiler.xsd.{ReferenceTypeSymbol, ComplexTypeDecl}

    val parts = paramMessage(input).part
    val typeSymbol = toTypeSymbol(parts.head)
    val decl = typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) => decl
      case x => error("unexpected type: " + x.toString)
    }
    val flatParticles = xsdgenerator.flattenElements(decl, 0)
    val paramList = flatParticles map { xsdgenerator.buildParam }
    val entity = "%s(%s)".format(xsdgenerator.buildTypeName(typeSymbol, true),
      paramList.map(_.toParamName).mkString("," + NL + indent(2)))
    """%s, Map()""".format(entity)
  }

  def elementRefToTypeName(ref: Option[String]): String = ref map { x =>
    val elem = xsdgenerator.elements(splitTypeName(x))
    xsdgenerator.buildTypeName(elem.typeSymbol, true)
  } getOrElse {"Any"}

  def makeBindingName(binding: XBindingType): String = {
    val name = xsdgenerator.makeTypeName(binding.name)
    if (name.endsWith("Binding")) name
    else name + "Binding"
  }

  def makeSoap11Binding(binding: XBindingType): Snippet = {
    val name = makeBindingName(binding)
    log("wsdl11#makeSoap11Binding: " + name)

    val interfaceType = context.interfaces(splitTypeName(binding.typeValue.toString))
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

    val operations = binding.operation map { opBinding => makeOperation(opBinding, interfaceType, document) }
    val bindingOps = binding.operation map { opBinding => makeSoap12OpBinding(opBinding, interfaceType, document) }

    val interfaceTrait = <source>
trait {interfaceTypeName} {{
  {operations.mkString(NL + "  ")}
}}
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
    val interfaceType = context.interfaces(splitTypeName(binding.typeValue.toString))
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
      port <- service.port if binding == context.bindings(splitTypeName(port.binding.toString))
    } yield port

  def elements(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     => throw new ReferenceNotFound("element" , namespace, name)
      }

  def splitTypeName(ref: String) = Module.splitTypeName(ref, scope)
}
