/*
 * Copyright (c) 2010 e.e d3si9n
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

package scalaxb.compiler.xsd
import scala.collection.mutable

trait XMLOutput extends Args {  
  def buildXMLString(param: Param): String = {
    val ns = quoteNamespace(param.namespace)
    val name = "__obj." + makeParamName(param.name)
    
    val typeAttribute = param.typeSymbol match {
      case XsAnyType => "true"
      case _         => "false"
    }
    
    val xToXMLCode = "x => " + buildToXML(param.baseTypeName, "x, x.namespace, x.key, __scope, " + typeAttribute)
    val toXMLCode = param.typeSymbol match {
      case XsAnyType            => xToXMLCode
      case XsNillableAny        => xToXMLCode
      case XsDataRecord(member) => xToXMLCode
      case _ => buildToXML(param.baseTypeName, "_, " + ns + ", " + quote(Some(param.name)) + 
        ", __scope, " + typeAttribute)
    }
    
    val elemLabel = param.typeSymbol match {
        case XsAnyType            => name + ".key"
        case XsNillableAny        => name + ".key"
        case XsDataRecord(member) => name + ".key"
        case _ => quote(Some(param.name))
      }
    
    val optionalType = "Option[" + param.baseTypeName + "]"
    val xOptionalToXMLCode = "x => " + buildToXML(optionalType, "x, x.namespace, x.key, __scope, " + typeAttribute)
    val optionalToXMLCode = param.typeSymbol match {
      case XsAnyType            => xOptionalToXMLCode
      case XsDataRecord(member) => xOptionalToXMLCode
      case _ => buildToXML(optionalType, "_, " + ns + ", " + quote(Some(param.name)) + ", __scope, " + typeAttribute)
    }
    
    val retval = (param.cardinality, param.nillable) match {
      case (Multiple, true)  => name + " flatMap { " + optionalToXMLCode + " }"
      case (Multiple, false) => name + " flatMap { " + toXMLCode + " }"
      case (Optional, true)  => name + " map { " +  optionalToXMLCode + " } getOrElse {Nil}" 
      case (Optional, false) => name + " map { " + toXMLCode + " } getOrElse {Nil}"
      case (Single, true)    => buildToXML(optionalType, name + ", " + ns + ", " + elemLabel + ", __scope, " + typeAttribute)
      case (Single, false)   => buildToXML(param.baseTypeName, name + ", " + ns + ", " + elemLabel + ", __scope, " + typeAttribute)
    }
    
    retval
  }
  
  def buildAttributeString(attr: AttributeLike): String = attr match {
    case ref: AttributeRef => buildAttributeString(buildAttribute(ref))
    case x: AttributeDecl  => buildAttributeString(x)
    case any: AnyAttributeDecl => buildAttributeString(any)
    case group: AttributeGroupDecl => buildAttributeString(group)
  }
  
  def buildAttributeString(any: AnyAttributeDecl): String =
    "__obj." + makeParamName(ATTRS_PARAM) + ".toList map {" + newline +
    indent(4) + "case (key, x) => attr = scala.xml.Attribute((x.namespace map { __scope.getPrefix(_) }).orNull, x.key.orNull, x.value.toString, attr) }"
    
  def buildAttributeString(attr: AttributeDecl): String = {
    val namespaceString = if (attr.global) "__scope.getPrefix(" + quote(attr.namespace.orNull) + ")"
      else "null"
    val name = "__obj." + makeParamName(buildParam(attr).name)
    
    if (toCardinality(attr) == Optional)
      name + " foreach { x => attr = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) +
        ", " + buildToString("x", attr.typeSymbol) + ", attr) }"
    else attr.defaultValue match {
      case Some(x) =>
        "if (" + buildToString(name, attr.typeSymbol) + " != " + quote(x) + 
        ") attr = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attr)"
      case None =>
        "attr = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attr)"
    }
  }
  
  def buildAttributeString(group: AttributeGroupDecl): String =
    "attr = " + buildFormatterName(group) + ".toAttribute(__obj." + makeParamName(buildParam(group).name) + ", attr, __scope)"
    
  def buildToString(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol if (buildTypeName(symbol) == "java.util.GregorianCalendar") ||
      (buildTypeName(symbol) == "Array[Byte]")  =>
      "scalaxb.Helper.toString(" + selector + ")"
    case symbol: BuiltInSimpleTypeSymbol => selector + ".toString"
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>       
      decl.content match {
        case x: SimpTypListDecl => selector + ".map(x => " + buildToString("x", baseType(decl)) + ").mkString(\" \")" 
        case _ => buildToString(selector, baseType(decl))
      }
    case _ => selector + ".toString"
  }
}
