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
    log("wsdl11: GenSource.generate: " + context.interfaces.toString)

    val interfaces = ((soap11Bindings(definition) ++ soap12Bindings(definition)).toList map { binding =>
      context.interfaces(splitTypeName(binding.typeValue.toString))
    }).distinct

    Snippet((interfaces map {makeInterface}) ++
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

  def makeInterface(intf: XPortTypeType): Snippet = {
    val name = intf.name.capitalize
    log("wsdl11#makeInterface: " + name)

    val operations = intf.operation map {makeOperation}

    val definition = <source>
trait {name} {{
  {operations.mkString(NL + "  ")}
}}
</source>
    Snippet(definition)
  }

  def makeOperation(op: XOperationType): String = {
    def arg(input: XParamType) = buildIRIStyleArg(input) map {_.toScalaCode} mkString(", ")

    val name = camelCase(op.name)
    log("wsdl11#makeOperation: " + name)

    val retval = op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) =>
        "def %s(%s): Unit".format(name, arg(input))
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(output))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        "def %s(%s): Either[scalaxb.Fault[%s], %s]".format(name, arg(input),
          faultsToTypeName(faults), outputTypeName(output))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        "def %s: %s".format(name, outputTypeName(output))
      case _ => error("unsupported.")
    }

    log("wsdl11#makeOperation: " + retval)
    retval
  }

  def makeSoap12OpBinding(binding: XBinding_operationType, intf: XPortTypeType): String = {
    val op = (intf.operation filter {_.name == binding.name}).headOption getOrElse {
      error("operation %s was not found in %s".format(binding.name, intf.name))
    }
    val address = "baseAddress"
    val quotedMethod = "\"POST\""
    val action = binding.any.headOption match {
      case Some(DataRecord(_, _, node: Node)) => (node \ "@soapAction").headOption map {_.text}
      case _ => None
    }
    val actionString = action map {"Some(new java.net.URI(\"%s\"))".format(_)} getOrElse {"None"}

    def faultString(faults: Seq[XFaultType]): String = faultsToTypeName(faults) match {
      case "Any" => "x"
      case x     => "x.asFault[%s]".format(x)
    }

    def outputString(output: XParamType): String = "scalaxb.fromXML[%s](x)%s".format(
      paramTypeName(output),
      singleOutputType(output) map { elem =>
        val param = xsdgenerator.buildParam(elem)
        "." + param.toParamName
      } getOrElse {""})

    val opImpl = op.xoperationtypeoption match {
      case DataRecord(_, _, XOnewayoperationSequence(input)) =>
        // "def %s(%s): Unit".format(op.name, arg(input))
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x)
          |          case Right(x) => ()
          |        }""".stripMargin.format(invokeToXML(input), address, quotedMethod, actionString)
      case DataRecord(_, _, XRequestresponseoperationSequence(input, output, faults)) =>
        // "def %s(%s): Option[scalaxb.Fault[%s]]".format(op.name, arg(input), faultsToTypeName(faults))
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right(x) => Right(%s)
          |        }""".stripMargin.format(
          invokeToXML(input), address, quotedMethod, actionString,
          faultString(faults), outputString(output))
      case DataRecord(_, _, XSolicitresponseoperationSequence(output, input, faults)) =>
        // "def %s(%s): Either[scalaxb.Fault[Any], %s]".format(op.name, arg(input), paramTypeName)
        """soapClient.requestResponse(%s, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => Left(%s)
          |          case Right(x) => Right(%s)
          |        }""".format(
          invokeToXML(input), address, quotedMethod, actionString,
          faultString(faults), outputString(output))
      case DataRecord(_, _, XNotificationoperationSequence(output)) =>
        // "def %s: %s".format(op.name, paramTypeName)
        """soapClient.requestResponse(Nil, defaultScope, %s, %s, %s) match {
          |          case Left(x)  => error(x)
          |          case Right(x) => %s
          |        }""".stripMargin.format(address, quotedMethod, actionString, outputString(output))
      case _ => error("unsupported.")
    }

    val retval = makeOperation(op) + " = " + NL +
      "        " + opImpl
    log(retval)
    retval
  }

  def invokeToXML(input: XParamType): String = {
    val entity = "%s(%s)".format(paramTypeName(input), buildIRIStyleArg(input) map {_.toParamName} mkString(", "))
    val inputElementLabel = "\"%s\"".format(toElement(paramMessage(input).part.head).name)
    "scalaxb.toXML(%s, targetNamespace, %s, defaultScope)".format(entity, inputElementLabel)
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

  case class ParamCache(toScalaCode: String, toParamName: String)

  def buildIRIStyleArg(input: XParamType): List[ParamCache] = paramMessage(input).part.headOption map { part =>
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
          ParamCache(param.toScalaCode, param.toParamName)
        }
      case x => error("unexpected type: " + x)
    }
  } getOrElse {error("unexpected input: " + input)}

  def buildPartsArg(input: XParamType): String = (paramMessage(input).part map { part =>
    "%s: %s".format(part.name getOrElse {"in"}, xsdgenerator.buildTypeName(toTypeSymbol(part), true))
  }).mkString(", ")

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
  } getOrElse {error("part does not have an element")}

  def outputTypeName(output: XParamType): String = singleOutputType(output) map { elem =>
    val param = xsdgenerator.buildParam(elem)
    param.typeName
  } getOrElse {paramTypeName(output)}

  def singleOutputType(output: XParamType): Option[scalaxb.compiler.xsd.ElemDecl] = paramMessage(output).part.headOption map { part =>
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

  def paramTypeName(param: XParamType): String =
    paramMessage(param).part.headOption map { part =>
      xsdgenerator.buildTypeName(toTypeSymbol(part), true)
    } getOrElse {"Any"}

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

  def makeSoap11Binding(binding: XBindingType): Snippet = {
    val name = binding.name.capitalize + "Binding"
    val interfaceType = context.interfaces(splitTypeName(binding.typeValue.toString))
    val interfaceTypeName = interfaceType.name.capitalize
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP11 =>
        Some((node \ "@location").text)
      case _ => None
    }}
    val addressString = address map {"""lazy val baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val bindingOps = binding.operation map { opBinding => makeSoap12OpBinding(opBinding, interfaceType) }
    val opString = bindingOps.mkString(NL + "      ")
    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.Soap11Clients =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(xsdgenerator.schema.targetNamespace) }
    lazy val service: {interfaceTypeName} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeName} {{
      {opString}
    }}
  }}
</source>
    Snippet(<source/>, <source/>, bindingTrait, <source/>)
  }

  // http://www.w3.org/TR/2007/REC-wsdl20-adjuncts-20070626/#soap-binding
  // http://www.w3.org/TR/2007/REC-soap12-part2-20070427/
  def makeSoap12Binding(binding: XBindingType): Snippet = {
    val name = binding.name.capitalize + "Binding"
    val interfaceType = context.interfaces(splitTypeName(binding.typeValue.toString))
    val interfaceTypeName = interfaceType.name.capitalize
    val port = findPort(binding).headOption
    val address = port flatMap {_.any flatMap {
      case DataRecord(_, _, node: Node) if node.scope.getURI((node.prefix)) == WSDL_SOAP12 =>
        Some((node \ "@location").text)
      case _ => None
    }}
    val addressString = address map {"""lazy val baseAddress = new java.net.URI("%s")""".format(_)} getOrElse {""}

    val bindingOps = binding.operation map { opBinding => makeSoap12OpBinding(opBinding, interfaceType) }
    val opString = bindingOps.mkString(NL + "      ")
    val bindingTrait = <source>
  trait {name}s {{ this: scalaxb.SoapClients =>
    lazy val targetNamespace: Option[String] = { xsdgenerator.quote(xsdgenerator.schema.targetNamespace) }
    lazy val service: {interfaceTypeName} = new {name} {{}}
    {addressString}

    trait {name} extends {interfaceTypeName} {{
      {opString}
    }}
  }}
</source>
    Snippet(<source/>, <source/>, bindingTrait, <source/>)
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
