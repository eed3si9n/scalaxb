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
  val attributes: List[AttributeLike]
}

abstract class ContentTypeDecl extends Decl

case class SimpContRestrictionDecl(base: XsTypeSymbol,
  facets: List[Facetable],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object SimpContRestrictionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, config)
            config.typeList += decl
            val symbol = new ReferenceTypeSymbol(decl.name)
            symbol.decl = decl
            symbol
          
          case None    => error("SimpContRestrictionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    
    val facets = Facetable.fromParent(node, config)
    
    var attributes: List[AttributeLike] = Nil
    for (child <- node.child) child match {
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeLike.fromXML(child, config) :: attributes

      case _ =>
    }
    
    SimpContRestrictionDecl(base, facets, attributes.reverse)
  }
}

case class SimpContExtensionDecl(base: XsTypeSymbol,
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object SimpContExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, config)
            config.typeList += decl
            val symbol = new ReferenceTypeSymbol(decl.name)
            symbol.decl = decl
            symbol
          
          case None    => error("SimpContExtensionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    var attributes: List[AttributeLike] = Nil

    for (child <- node.child) child match {
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeLike.fromXML(child, config) :: attributes

      case _ =>
    }

    SimpContExtensionDecl(base, attributes.reverse)
  }  
}
case class CompContRestrictionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent
  
object CompContRestrictionDecl {
  def empty =
    CompContRestrictionDecl(XsAny, None, Nil)
  
  def fromAttributes(attributes: List[AttributeLike]) = 
    CompContRestrictionDecl(XsAny, None, attributes)
  
  def fromCompositor(compositor: HasParticle, attributes: List[AttributeLike]) =
    CompContRestrictionDecl(XsAny, Some(compositor), attributes)
  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: Option[HasParticle] = None
    var attributes: List[AttributeLike] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <all>{ _* }</all> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeLike.fromXML(child, config) :: attributes
      
      case _ =>
    }
    
    CompContRestrictionDecl(base, compositor, attributes.reverse)
  }
}

case class CompContExtensionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object CompContExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: Option[HasParticle] = None
    var attributes: List[AttributeLike] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <all>{ _* }</all> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(CompositorDecl.fromXML(child, config))
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeLike.fromXML(child, config) :: attributes
      
      case _ =>
    }
       
    CompContExtensionDecl(base, compositor, attributes.reverse)
  }  
}

case class SimpTypRestrictionDecl(base: XsTypeSymbol, facets: List[Facetable]) extends ContentTypeDecl

object SimpTypRestrictionDecl {  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, config)
            config.typeList += decl
            val symbol = new ReferenceTypeSymbol(decl.name)
            symbol.decl = decl
            symbol
          
          case None    => error("SimpTypRestrictionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    
    val facets = Facetable.fromParent(node, config)
    SimpTypRestrictionDecl(base, facets)
  }
}

case class SimpTypListDecl(itemType: XsTypeSymbol) extends ContentTypeDecl

object SimpTypListDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val itemType = (node \ "@itemType").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, config)
            config.typeList += decl
            val symbol = new ReferenceTypeSymbol(decl.name)
            symbol.decl = decl
            symbol
          
          case None    => error("SimpTypListDecl#fromXML: restriction must have either itemType attribute or simpleType.")
        }
    }
    
    SimpTypListDecl(itemType)
  }
}

case class SimpTypUnionDecl() extends ContentTypeDecl

object SimpTypUnionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    SimpTypUnionDecl()
  }
}

trait Facetable {
  val value: String
}

object Facetable {
  def fromParent(node: scala.xml.Node, config: ParserConfig): List[Facetable] = 
    node.child.toList collect {
      case x@(<enumeration>{ _* }</enumeration>) => EnumerationDecl.fromXML(x, config)
    }
}

case class EnumerationDecl(value: String) extends Facetable

object EnumerationDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) =
    EnumerationDecl((node \ "@value").text)
}
