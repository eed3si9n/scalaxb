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

import scalashim._

trait ComplexTypeContent {
  val base: XsTypeSymbol
  val attributes: List[AttributeLike]
}

abstract class ContentTypeDecl extends Decl

case class SimpContRestrictionDecl(base: XsTypeSymbol, simpleType: Option[XsTypeSymbol],
  facets: List[Facetable[_]],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object SimpContRestrictionDecl {
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val simpleType = (node \ "simpleType").headOption map { x =>
      val decl = SimpleTypeDecl.fromXML(x, family, config)
      config.typeList += decl
      val symbol = ReferenceTypeSymbol(config.targetNamespace, decl.name)
      symbol.decl = decl
      symbol
    }
    
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, node.scope, config)
      case None    =>
        simpleType getOrElse {
          sys.error("SimpContRestrictionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    
    val facets = Facetable.fromParent(node, base, config)
    val attributes = AttributeLike.fromParentNode(node, config)
    SimpContRestrictionDecl(base, simpleType, facets, attributes)
  }
}

case class SimpContExtensionDecl(base: XsTypeSymbol,
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object SimpContExtensionDecl {
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, node.scope, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, family, config)
            config.typeList += decl
            val symbol = ReferenceTypeSymbol(config.targetNamespace, decl.name)
            symbol.decl = decl
            symbol
          
          case None    => sys.error("SimpContExtensionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    val attributes = AttributeLike.fromParentNode(node, config)
    SimpContExtensionDecl(base, attributes)
  }  
}
case class CompContRestrictionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent
  
object CompContRestrictionDecl {
  def empty =
    CompContRestrictionDecl(XsAnyType, None, Nil)
  
  def fromAttributes(attributes: List[AttributeLike]) = 
    CompContRestrictionDecl(XsAnyType, None, attributes)
  
  def fromCompositor(compositor: HasParticle, attributes: List[AttributeLike]) =
    CompContRestrictionDecl(XsAnyType, Some(compositor), attributes)
  
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, node.scope, config)
    var compositor: Option[HasParticle] = None
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <all>{ _* }</all> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))

      case _ =>
    }

    val attributes = AttributeLike.fromParentNode(node, config)
    CompContRestrictionDecl(base, compositor, attributes)
  }
}

case class CompContExtensionDecl(base: XsTypeSymbol,
  compositor: Option[HasParticle],
  attributes: List[AttributeLike]) extends ContentTypeDecl with ComplexTypeContent

object CompContExtensionDecl {
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, node.scope, config)
    var compositor: Option[HasParticle] = None
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <all>{ _* }</all> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <choice>{ _* }</choice> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case <sequence>{ _* }</sequence> =>
        compositor = Some(CompositorDecl.fromXML(child, family, config))
      case _ =>
    }

    val attributes = AttributeLike.fromParentNode(node, config)
    CompContExtensionDecl(base, compositor, attributes)
  }  
}

case class SimpTypRestrictionDecl(base: XsTypeSymbol, facets: List[Facetable[_]]) extends ContentTypeDecl

object SimpTypRestrictionDecl {  
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val base = (node \ "@base").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, node.scope, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, family, config)
            config.typeList += decl
            val symbol = ReferenceTypeSymbol(config.targetNamespace, decl.name)
            symbol.decl = decl
            symbol
          
          case None    => sys.error("SimpTypRestrictionDecl#fromXML: restriction must have either base attribute or simpleType.")
        }
    }
    
    val facets = Facetable.fromParent(node, base, config)
    SimpTypRestrictionDecl(base, facets)
  }
}

case class SimpTypListDecl(itemType: XsTypeSymbol) extends ContentTypeDecl

object SimpTypListDecl {
  def fromXML(node: scala.xml.Node, family: List[String], config: ParserConfig) = {
    val itemType = (node \ "@itemType").headOption match {
      case Some(x) => TypeSymbolParser.fromString(x.text, node.scope, config)
      case None    =>
        (node \ "simpleType").headOption match {
          case Some(x) => Some(x.text)
            val decl = SimpleTypeDecl.fromXML(x, family, config)
            config.typeList += decl
            val symbol = ReferenceTypeSymbol(config.targetNamespace, decl.name)
            symbol.decl = decl
            symbol
          
          case None    => sys.error("SimpTypListDecl#fromXML: restriction must have either itemType attribute or simpleType.")
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

trait Facetable[A] {
  val value: A
}

object Facetable {
  def fromParent(node: scala.xml.Node, base: XsTypeSymbol, config: ParserConfig): List[Facetable[_]] = 
    node.child.toList collect {
      case x@(<enumeration>{ _* }</enumeration>) => EnumerationDecl.fromXML(x, base, config)
    }
}

case class EnumerationDecl[A](value: A) extends Facetable[A]

object EnumerationDecl {
  def fromXML(node: scala.xml.Node, base: XsTypeSymbol, config: ParserConfig) = {
    def secureName(name: String): String = s"EnumValue_$name"
    base match {
      case XsQName =>
        val (ns, localPart) = TypeSymbolParser.splitTypeName(secureName((node \ "@value").text), config.scope, config.targetNamespace)
        val qname = new javax.xml.namespace.QName(ns.orNull, localPart)
        EnumerationDecl(qname)
      case _ => EnumerationDecl(secureName((node \ "@value").text))
    }
  }
}
