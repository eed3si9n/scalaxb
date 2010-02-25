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

package scalaxb.xsd

import scalaxb.{Main}
import scala.xml.{TypeSymbol}
import scala.collection.{Map, Set}
import scala.collection.mutable
import scala.collection.immutable

abstract class Decl

class ParserConfig {
  var xsPrefix: String = "xsd:"
  var myPrefix: String = ""
  val topElems  = mutable.ListMap.empty[String, ElemDecl]
  val elemList  = mutable.ListBuffer.empty[ElemDecl]
  val types     = mutable.ListMap.empty[String, TypeDecl]
  val attrs     = mutable.ListMap.empty[String, AttributeDecl]
  val choices   = mutable.Set.empty[ChoiceDecl]
}

object DefaultParserConfig extends ParserConfig

case class SchemaDecl(topElems: Map[String, ElemDecl],
    elemList: List[ElemDecl],
    types: Map[String, TypeDecl],
    choices: Set[ChoiceDecl]) {
  
  val newline = System.getProperty("line.separator")
  
  override def toString: String = {
    "SchemaDecl(topElems(" + topElems.valuesIterator.mkString("," + newline) + "),types(" +
      types.valuesIterator.mkString("," + newline)  + "))"
  }
}

object SchemaDecl {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig = DefaultParserConfig) = {
    val XML_SCHEMA_URI = "http://www.w3.org/2001/XMLSchema"
    
    val schema = (node \\ "schema").headOption match {
      case Some(x) => x
      case None    => throw new Exception("xsd: schema element not found: " + node.toString)
    }
    
    val xsPrefix = schema.scope.getPrefix(XML_SCHEMA_URI)
    if (xsPrefix != null) {
      config.xsPrefix = xsPrefix + ":"
    }
    schema.attribute("targetNamespace") match {
      case Some(x) =>
        val myPrefix = schema.scope.getPrefix(x.text)
        if (myPrefix != null) {
          config.myPrefix = myPrefix + ":"
        } 
      case None    =>
    }
    
    for (node <- schema \ "element";
        if (node \ "@name").headOption.isDefined) {
      val elem = ElemDecl.fromXML(node, config)
      config.topElems += (elem.name -> elem)
    }
    
    for (node <- schema \\ "complexType";
        if (node \ "@name").headOption.isDefined)
      ComplexTypeDecl.fromXML(node, (node \ "@name").text, config)
      
    (schema \\ "simpleType").foreach(SimpleTypeDecl.fromXML(_, config))
    
    resolveType(config)
    
    SchemaDecl(immutable.ListMap.empty[String, ElemDecl] ++ config.topElems,
      config.elemList.toList,
      immutable.ListMap.empty[String, TypeDecl] ++ config.types,
      config.choices)
  }
  
  def resolveType(config: ParserConfig) {    
    for (elem <- config.elemList)
      resolveType(elem.typeSymbol, config)
             
    for (attr <- config.attrs.valuesIterator) {
      attr.typeSymbol match {
        case symbol: BuiltInSimpleTypeSymbol =>
        
        case symbol: ReferenceTypeSymbol =>
          if (!config.types.contains(symbol.name))
            throw new Exception("SchemaDecl: type not found " + attr.name + ": " + symbol.name)
          config.types(symbol.name) match {
            case decl: SimpleTypeDecl => symbol.decl = decl
            case _ => throw new Exception("SchemaDecl: type does not match ")
          } // match
      } // match    
    } // for
    
    for (typ <- config.types.valuesIterator) typ match {
      case decl: SimpleTypeDecl => // do nothing
            
      case ComplexTypeDecl(_, SimpleContentDecl(res: SimpContRestrictionDecl), _) =>
        resolveType(res.base, config)
      
      case ComplexTypeDecl(_, SimpleContentDecl(ext: SimpContExtensionDecl), _) =>
        resolveType(ext.base, config)
      
      case ComplexTypeDecl(_, ComplexContentDecl(res: CompContRestrictionDecl), _) =>
        resolveType(res.base, config)
      
      case ComplexTypeDecl(_, ComplexContentDecl(ext: CompContExtensionDecl), _) =>
        resolveType(ext.base, config)
        
      case _ =>
    }
  }
  
  def resolveType(value: XsTypeSymbol, config: ParserConfig): Unit = value match {
    case symbol: ReferenceTypeSymbol =>
      if (!config.types.contains(symbol.name))
        throw new Exception("SchemaDecl: type not found: " + symbol.name)
      
      if (symbol.decl == null)
        symbol.decl = config.types(symbol.name)
      
    case symbol: BuiltInSimpleTypeSymbol => // do nothing 
    case xsAny => // do nothing
  } // match

}

case class AnnotationDecl() extends Decl

object AnnotationDecl {
  def fromXML(node: scala.xml.Node) = AnnotationDecl() 
}

abstract class AttributeUse
object OptionalUse extends AttributeUse
object ProhibitedUse extends AttributeUse
object RequiredUse extends AttributeUse

case class AttributeDecl(name: String,
    typeSymbol: XsTypeSymbol,
    defaultValue: Option[String],
    fixedValue: Option[String],
    use: AttributeUse) extends Decl {
  override def toString = "@" + name
}

object AttributeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    if (!(node \ "@ref").isEmpty) {
      val ref = (node \ "@ref").text.replaceFirst(config.myPrefix, "")
      
      Main.log("AttributeDelc.fromXML: " + ref)
      if (!config.attrs.contains(ref)) {
        throw new Exception("xsd: Attribute ref not found " + ref)
      }
      
      config.attrs(ref)
    } else {
      val name = (node \ "@name").text
      Main.log("AttributeDelc.fromXML: " + name)
      var typeSymbol: XsTypeSymbol = xsUnknown
      val typeName = (node \ "@type").text
      if (typeName != "") {
        typeSymbol = TypeSymbolParser.fromString(typeName, config)
      } else {
        for (child <- node.child) child match {
          case <simpleType>{ _* }</simpleType> =>
            typeSymbol = new ReferenceTypeSymbol(SimpleTypeDecl.buildName(child))
          case _ =>
        }
      } // if-else
      
      val defaultValue = (node \ "@default").headOption match {
        case None    => None
        case Some(x) => Some(x.text)
      }
      val fixedValue = (node \ "@fixed").headOption match {
        case None    => None
        case Some(x) => Some(x.text)
      }
      val use = (node \ "@use").text match {
        case "prohibited" => ProhibitedUse
        case "required"   => RequiredUse
        case _            => OptionalUse
      }
      
      val attr = AttributeDecl(name, typeSymbol,
        defaultValue, fixedValue, use)
      config.attrs += (attr.name -> attr)
      attr   
    }
  } 
}

case class ElemRef(ref: String,
  minOccurs: Option[Int],
  maxOccurs: Option[Int]) extends Decl

object ElemRef {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val ref = (node \ "@ref").text.replaceFirst(config.myPrefix, "")
    Main.log("ElemRef.fromXML: ref " + ref)
    
    val minOccurs = (node \ "@minOccurs").headOption match {
      case None    => None
      case Some(x) => Some(CompositorDecl.buildOccurrence((node \ "@minOccurs").text))
    }
    
    val maxOccurs = (node \ "@maxOccurs").headOption match {
      case None    => None
      case Some(x) => Some(CompositorDecl.buildOccurrence((node \ "@maxOccurs").text))
    }
    
    ElemRef(ref, minOccurs, maxOccurs)
  }
}

case class ElemDecl(name: String,
  typeSymbol: XsTypeSymbol,
  defaultValue: Option[String],
  fixedValue: Option[String],  
  minOccurs: Int,
  maxOccurs: Int) extends Decl

object ElemDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val name = (node \ "@name").text
    Main.log("ElemDecl.fromXML: name " + name)
    var typeSymbol: XsTypeSymbol = xsAny
    val typeName = (node \ "@type").text
    
    if (typeName != "") {
      typeSymbol = TypeSymbolParser.fromString(typeName, config)
    } else {
      for (child <- node.child) child match {
        case <complexType>{ _* }</complexType> =>
          val decl = ComplexTypeDecl.fromXML(child, "complexType@" + name, config)
          config.types += (decl.name -> decl)
          val symbol = new ReferenceTypeSymbol(decl.name)
          symbol.decl = decl
          typeSymbol = symbol
          
        case <simpleType>{ _* }</simpleType> =>
          typeSymbol = new ReferenceTypeSymbol(SimpleTypeDecl.buildName(child))
                      
        case _ =>
      }
    } // if-else
    
    val defaultValue = (node \ "@default").headOption match {
      case None    => None
      case Some(x) => Some(x.text)
    }
    val fixedValue = (node \ "@fixed").headOption match {
      case None    => None
      case Some(x) => Some(x.text)
    }      
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    
    val elem = ElemDecl(name, typeSymbol, defaultValue, fixedValue, minOccurs, maxOccurs)
    config.elemList += elem
    elem
  }
}


abstract class TypeDecl extends Decl

/** simple types cannot have element children or attributes.
 */
case class SimpleTypeDecl(name: String, content: ContentTypeDecl) extends TypeDecl {
  override def toString(): String = name + "(" + content.toString + ")"
}

object SimpleTypeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    Main.log("SimpleTypeDecl.fromXML: " + node.toString)
    
    val name = buildName(node)
    
    var content: ContentTypeDecl = null
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction>  => content = SimpTypRestrictionDecl.fromXML(child, config) 
      case <list>{ _* }</list>                => content = SimpTypListDecl.fromXML(child, config)
      case <union>{ _* }</union>              => content = SimpTypUnionDecl.fromXML(child, config)
      case _ =>     
    }
    
    val typ = SimpleTypeDecl(name, content)
    config.types += (typ.name -> typ) 
    typ
  }
  
  def buildName(node: scala.xml.Node) = {
    val name = (node \ "@name").text
    if (name != "")
      name
    else
      "simpleType@" + node.hashCode.toString
  }
}

/** complex types may have element children and attributes.
 */
case class ComplexTypeDecl(name: String,
  content: HasComplexTypeContent,
  attributes: List[AttributeDecl]) extends TypeDecl

object ComplexTypeDecl {  
  def fromXML(node: scala.xml.Node, name: String, config: ParserConfig) = {
    Main.log("ComplexTypeDecl.fromXML: " + node.toString)
    var content: HasComplexTypeContent = ComplexContentDecl.empty
    
    val attributes = (node \ "attribute").toList.map(
      AttributeDecl.fromXML(_, config))
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        throw new Exception("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        content = ComplexContentDecl.fromCompositor(
          AllDecl.fromXML(child, config), attributes)
      case <choice>{ _* }</choice> =>
        content = ComplexContentDecl.fromCompositor(
          ChoiceDecl.fromXML(child, config), attributes)
      case <sequence>{ _* }</sequence> =>
        content = ComplexContentDecl.fromCompositor(
          SequenceDecl.fromXML(child, config), attributes)
      case <simpleContent>{ _* }</simpleContent> =>
        content = SimpleContentDecl.fromXML(child, config)
      case <complexContent>{ _* }</complexContent> =>
        content = ComplexContentDecl.fromXML(child, config)
      case _ =>
    }
    
    // val contentModel = ContentModel.fromSchema(firstChild(node))
    val typ = ComplexTypeDecl(name, content, attributes.reverse)
    config.types += (typ.name -> typ) 
    typ
  }
  
  def firstChild(node: scala.xml.Node) = {
    val children = for (child <- node.child; if child.isInstanceOf[scala.xml.Elem])
      yield child
    if (children.length == 0) {
      throw new Exception("there are no children: " + node.toString)
    }
      
    children(0)
  }
}

trait HasComplexTypeContent {
  val content: ComplexTypeContent
}

trait HasContent {
  val content: ContentTypeDecl
}

/** complex types with simple content only allow character content.
 */
case class SimpleContentDecl(content: ComplexTypeContent) extends Decl with HasComplexTypeContent

object SimpleContentDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    var content: ComplexTypeContent = null
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = SimpContRestrictionDecl.fromXML(child, config)
      case <extension>{ _* }</extension> =>
        content = SimpContExtensionDecl.fromXML(child, config)
      case _ =>
    }
       
    SimpleContentDecl(content)
  }
}

/** only complex types with complex content allow child elements
 */
case class ComplexContentDecl(content: ComplexTypeContent) extends Decl with HasComplexTypeContent

object ComplexContentDecl {
  lazy val empty =
    ComplexContentDecl(CompContRestrictionDecl.empty)
  
  def fromCompositor(compositor: HasParticle, attributes: List[AttributeDecl]) =
    ComplexContentDecl(CompContRestrictionDecl.fromCompositor(compositor, attributes))
  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    var content: ComplexTypeContent = CompContRestrictionDecl.empty
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = CompContRestrictionDecl.fromXML(child, config)
      case <extension>{ _* }</extension> =>
        content = CompContExtensionDecl.fromXML(child, config)
      case _ =>
    }
    
    ComplexContentDecl(content)
  }
}

abstract class CompositorDecl extends Decl

object CompositorDecl {
  def fromNodeSeq(seq: scala.xml.NodeSeq, config: ParserConfig) = {
    for (child <- seq.toList;
        if (child.isInstanceOf[scala.xml.Elem]) && (child.label != "attribute"))
      yield fromXML(child, config)
  }
  
  def fromXML(node: scala.xml.Node, config: ParserConfig): Decl = node match {
    case <element>{ _* }</element>   =>
      if ((node \ "@name").headOption.isDefined)
        ElemDecl.fromXML(node, config)
      else if ((node \ "@ref").headOption.isDefined)
        ElemRef.fromXML(node, config)
      else
        throw new Exception("xsd: Unspported content type " + node.toString) 
    case <choice>{ _* }</choice>     => ChoiceDecl.fromXML(node, config)
    case <sequence>{ _* }</sequence> => SequenceDecl.fromXML(node, config)
    case <all>{ _* }</all>           => AllDecl.fromXML(node, config)
    
    case _ => throw new Exception("xsd: Unspported content type " + node.label)   
  }
  
  def buildOccurrence(value: String) =
    if (value == "")
      1
    else if (value == "unbounded")
      Integer.MAX_VALUE
    else
      value.toInt
}

trait HasParticle {
  val particles: List[Decl]
  val minOccurs: Int
  val maxOccurs: Int
}

case class SequenceDecl(particles: List[Decl],
  minOccurs: Int,
  maxOccurs: Int) extends CompositorDecl with HasParticle

object SequenceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    SequenceDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
  }
}

case class ChoiceDecl(particles: List[Decl],
  minOccurs: Int,
  maxOccurs: Int) extends CompositorDecl with HasParticle

object ChoiceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    val choice = ChoiceDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
    config.choices += choice
    choice
  }
}

case class AllDecl(particles: List[Decl],
  minOccurs: Int,
  maxOccurs: Int) extends CompositorDecl with HasParticle

object AllDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    AllDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
  }
}

object TypeSymbolParser {  
  def fromString(name: String, config: ParserConfig): XsTypeSymbol = {
    val xsType = XsTypeSymbol.toTypeSymbol(name.replaceFirst(config.xsPrefix, ""))
    if (xsType != xsUnknown) {
      xsType
    } else {
      val n = name.replaceFirst(config.myPrefix, "")
      new ReferenceTypeSymbol(n)
    }
  }
}
