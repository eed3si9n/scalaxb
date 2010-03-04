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

trait ComplexTypeContent {
  val base: XsTypeSymbol
  val attributes: List[AttributeDecl]
}

abstract class ContentTypeDecl extends Decl

case class SimpContRestrictionDecl(base: XsTypeSymbol,
  attributes: List[AttributeDecl]) extends ContentTypeDecl with ComplexTypeContent

object SimpContRestrictionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var attributes: List[AttributeDecl] = Nil

    for (child <- node.child) child match {
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes

      case _ =>
    }

    SimpContRestrictionDecl(base, attributes.reverse)
  }
}

case class SimpContExtensionDecl(base: XsTypeSymbol,
  attributes: List[AttributeDecl]) extends ContentTypeDecl with ComplexTypeContent

object SimpContExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var attributes: List[AttributeDecl] = Nil

    for (child <- node.child) child match {
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes

      case _ =>
    }

    SimpContExtensionDecl(base, attributes.reverse)
  }  
}
case class CompContRestrictionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeDecl]) extends ContentTypeDecl with ComplexTypeContent
  
object CompContRestrictionDecl {
  def empty =
    CompContRestrictionDecl(xsAny, None, Nil)
  
  def fromCompositor(compositor: HasParticle, attributes: List[AttributeDecl]) =
    CompContRestrictionDecl(xsAny, Some(compositor), attributes)
  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: Option[HasParticle] = None
    var attributes: List[AttributeDecl] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        error("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        compositor = Some(AllDecl.fromXML(child, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(ChoiceDecl.fromXML(child, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(SequenceDecl.fromXML(child, config))
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes
      
      case _ =>
    }
    
    CompContRestrictionDecl(base, compositor, attributes.reverse)
  }
}

case class CompContExtensionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeDecl]) extends ContentTypeDecl with ComplexTypeContent

object CompContExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: Option[HasParticle] = None
    var attributes: List[AttributeDecl] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        error("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        compositor = Some(AllDecl.fromXML(child, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(ChoiceDecl.fromXML(child, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(SequenceDecl.fromXML(child, config))
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes
      
      case _ =>
    }
       
    CompContExtensionDecl(base, compositor, attributes.reverse)
  }  
}

case class SimpTypRestrictionDecl(base: XsTypeSymbol) extends ContentTypeDecl

object SimpTypRestrictionDecl {  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    SimpTypRestrictionDecl(base)
  }
}

case class SimpTypListDecl() extends ContentTypeDecl

object SimpTypListDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    SimpTypListDecl()
  }
}

case class SimpTypUnionDecl() extends ContentTypeDecl

object SimpTypUnionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    SimpTypUnionDecl()
  }
}
