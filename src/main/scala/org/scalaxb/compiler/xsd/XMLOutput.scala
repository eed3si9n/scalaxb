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

package org.scalaxb.compiler.xsd
import scala.collection.mutable

trait XMLOutput extends Params {
  def buildXMLString(param: Param): String = param.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildXMLStringForSimpleType(param)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildXMLStringForSimpleType(param)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildXMLStringForComplexType(param)
    case XsAny => buildXMLStringForChoiceWrapper(param)
    case XsInterNamespace => buildXMLStringForComplexType(param)
    case r: XsDataRecord => buildXMLStringForChoiceWrapper(param)
    case _ => error("GenSource#buildXMLString: " + param.toString +
      " Invalid type " + param.typeSymbol.getClass.toString + ": " + param.typeSymbol.toString)    
  }
      
  def buildXMLStringForChoiceWrapper(param: Param) = {
    val typeName = buildTypeName(param.typeSymbol)
    val name = "__obj." + makeParamName(param.name)
    val baseTypeName = param.typeSymbol match {
      case XsDataRecord(ReferenceTypeSymbol(x: ComplexTypeDecl)) =>
        val compositor = compositorWrapper(x)
        compositor match {
          case group: GroupDecl =>
            val primary = primaryCompositor(group)
            makeTypeName(context.compositorNames(primary))
          case _ => makeTypeName(context.compositorNames(compositor))
        }
      
      case _ => "rt.DataRecord"
    }
    val ns = quoteNamespace(param.namespace)
    
    val retval = (param.cardinality, param.nillable) match {
      case (Multiple, true) =>
        name + ".flatMap(x => x match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope)" + newline +
          indent(4) + "} )"        
      case (Multiple, false) =>    
        name + ".flatMap { x => " + baseTypeName + ".toXML(x, x.namespace, x.key, __scope) }"
      case (Optional, true) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"    
      case (Optional, false) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, x.namespace, x.key, __scope)" + newline +
          indent(5) + "case None => Nil" + newline +
          indent(4) + "}"
      case (Single, true) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, " + ns + ", " +
            makeParamName(param.name) + ".key, __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"
      case (Single, false) =>
          baseTypeName + ".toXML(" + name + ", " + ns + ", " +
            name + ".key, __scope)"
    }
    
    retval
  } 
  
  def buildXMLStringForComplexType(param: Param) = {
    val name = "__obj." + makeParamName(param.name)
    val baseTypeName = param.baseTypeName
    val ns = quoteNamespace(param.namespace)
    
    val retval = (param.cardinality, param.nillable)  match {
      case (Multiple, true) =>
        name + ".flatMap(x => x match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, " + ns + ", " + 
            quote(Some(param.name)) + ", __scope)" + newline +
          indent(5) + "case None => rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope)" + newline +
          indent(4) + "} )"        
      case (Multiple, false) =>
        name + ".flatMap(x => " + baseTypeName + ".toXML(x, " + ns + ", " + 
          quote(Some(param.name)) + ", __scope))"
      case (Optional, true) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, " + ns + ", " + 
            quote(Some(param.name)) + ", __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"    
      case (Optional, false) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, " + ns + ", " + 
            quote(Some(param.name)) + ", __scope)" + newline +
          indent(5) + "case None => Nil" + newline +
          indent(4) + "}"  
      case (Single, true) =>
        name + " match {" + newline +
          indent(5) + "case Some(x) => " + baseTypeName + ".toXML(x, " + ns + ", " + 
            quote(Some(param.name)) + ", __scope)" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"
      case (Single, false) =>
        baseTypeName + ".toXML(" + name + ", " + ns + ", " + 
          quote(Some(param.name)) + ", __scope)"
    }
    
    retval
  }
  
  def buildToString(selector: String, typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol if (buildTypeName(symbol) == "java.util.GregorianCalendar") ||
      (buildTypeName(symbol) == "Array[Byte]")  =>
      "rt.Helper.toString(" + selector + ")"
    case symbol: BuiltInSimpleTypeSymbol => selector + ".toString"
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildToString(selector, decl)
    case _ => selector + ".toString"
  }
  
  def buildToString(selector: String, decl: SimpleTypeDecl): String = decl.content match {
    case x: SimpTypListDecl => selector + ".map(x => " + buildToString("x", baseType(decl)) + ").mkString(\" \")" 
    case _ => buildToString(selector, baseType(decl))
  }
  
  def buildXMLStringForSimpleType(param: Param) = {
    val ns = quoteNamespace(param.namespace)
    val prefix = "rt.Helper.getPrefix(" + ns + ", __scope).orNull"
    val retval = (param.cardinality, param.nillable) match {
      case (Multiple, true) =>
        "__obj." + makeParamName(param.name) + " collect {" + newline +
          indent(5) + "case Some(x) => scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " + 
            "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + "))" + newline +
          indent(5) + "case None =>   rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope)" + newline +
          indent(4) + "}"
      case (Multiple, false) => 
        "__obj." + makeParamName(param.name) + " map { x => scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " +
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")) }"
      case (Optional, true) =>
        "__obj." + makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => Seq(scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " + 
            "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"    
      case (Optional, false) =>
        "__obj." + makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => Seq(scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " + 
            "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
          indent(5) + "case None => Nil" + newline +
          indent(4) + "}"
      case (Single, true) =>    
        "__obj." + makeParamName(param.name) + " match {" + newline +
          indent(5) + "case Some(x) => Seq(scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " + 
            "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("x", param.typeSymbol) + ")))" + newline +
          indent(5) + "case None => Seq(rt.Helper.nilElem(" + ns + ", " + 
            quote(param.name) + ", __scope))" + newline +
          indent(4) + "}"
      case (Single, false) =>
        "scala.xml.Elem(" + prefix + ", " + quote(param.name) + ", " +
          "scala.xml.Null, __scope, scala.xml.Text(" + buildToString("__obj." + makeParamName(param.name), param.typeSymbol) + "))"   
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
    "__obj.anyAttribute.foreach { x =>" + newline +
    indent(3) + "attribute = scala.xml.Attribute((x.namespace map { __scope.getPrefix(_) }).orNull, x.key.orNull, x.value, attribute) }"
    
  def buildAttributeString(attr: AttributeDecl): String = {
    val namespaceString = if (attr.global)
      "__scope.getPrefix(" + quote(attr.namespace.orNull) + ")"
    else "null"
    val name = "__obj." + makeParamName(buildParam(attr).name)
        
    if (toMinOccurs(attr) == 0)
      name + " foreach { x =>" + newline +
      indent(3) + "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) +
        ", " + buildToString("x", attr.typeSymbol) + ", attribute) }"
    else attr.defaultValue match {
      case Some(x) =>
        "if (" + buildToString(name, attr.typeSymbol) + " != " + quote(x) + ") " + newline +
        indent(3) + "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attribute)"
      case None =>
        "attribute = scala.xml.Attribute(" + namespaceString + ", " + quote(attr.name) + ", " + 
        buildToString(name, attr.typeSymbol) + ", attribute)"
    }
  }
  
  def buildAttributeString(group: AttributeGroupDecl): String =
    "attribute = " + buildParam(group).baseTypeName + ".toAttribute(__obj." + makeParamName(buildParam(group).name) +
    ", attribute, __scope)"
}
