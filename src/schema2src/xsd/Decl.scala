/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

import scala.xml.{TypeSymbol}
import scala.collection.{Map}
import schema2src.{Main}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuilder
import scala.collection.immutable

abstract class Decl

class ParserConfig {
  var xsPrefix: String = "xsd:"
  var myPrefix: String = ""
  var elems: mutable.Map[String, ElemDecl] = null
  var types: mutable.Map[String, TypeDecl] = null
  var attrs: mutable.Map[String, AttributeDecl] = null
}

object DefaultParserConfig extends ParserConfig

case class SchemaDecl(elems: Map[String, ElemDecl], types: Map[String, TypeDecl]) {
  override def toString(): String = {
    "SchemaDecl(" + elems.valuesIterator.mkString(",") + "," +
      types.valuesIterator.mkString(",")  + ")"
  }
}

object SchemaDecl {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig = DefaultParserConfig) = {
    config.elems = mutable.Map.empty[String, ElemDecl]
    config.types = mutable.Map.empty[String, TypeDecl]
    config.attrs = mutable.Map.empty[String, AttributeDecl]
    
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
    
    (schema \ "element").foreach(ElemDecl.fromXML(_, config))
    (schema \\ "complexType").foreach(ComplexTypeDecl.fromXML(_, config))
    (schema \\ "simpleType").foreach(SimpleTypeDecl.fromXML(_, config))
    
    resolveType(config)
    
    SchemaDecl(immutable.Map.empty[String, ElemDecl] ++ config.elems,
      immutable.Map.empty[String, TypeDecl] ++ config.types)
  }
  
  def resolveType(config: ParserConfig) {
    config.elems.valuesIterator.foreach(
      elem => resolveType(elem.typeSymbol, config))
    
    for (elem <- config.elems.valuesIterator) {      
      elem.typeSymbol match {
        case symbol: BuiltInSimpleTypeSymbol =>
        
        case symbol: ReferenceTypeSymbol =>
          if (!config.types.contains(symbol.name))
            throw new Exception("SchemaDecl: type not found " + elem.name + ": " + symbol.name)
          if (symbol.decl == null)
            throw new Exception("SchemaDecl: type was found, but not mapped!!! " + elem.name + ": " + symbol.name)
      } // match    
    } // for
         
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
      case ComplexTypeDecl(_, content: CompositorContentDecl, _) =>
        // do nothing
      
      case ComplexTypeDecl(_, SimpleContentDecl(restriction: RestrictionDecl), _) =>
        resolveType(restriction.base, config)
      
      case ComplexTypeDecl(_, SimpleContentDecl(extension: ExtensionDecl), _) =>
        resolveType(extension.base, config)
      
      case ComplexTypeDecl(_, ComplexContentDecl(restriction: RestrictionDecl), _) =>
        resolveType(restriction.base, config)
      
      case ComplexTypeDecl(_, ComplexContentDecl(extension: ExtensionDecl), _) =>
        resolveType(extension.base, config)
    }
  }
  
  def resolveType(value: XsTypeSymbol, config: ParserConfig): Unit = value match {
    case symbol: BuiltInSimpleTypeSymbol =>
      
    case symbol: ReferenceTypeSymbol =>
      if (!config.types.contains(symbol.name))
        throw new Exception("SchemaDecl: type not found: " + symbol.name)
      symbol.decl = config.types(symbol.name)
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

case class ElemDecl(name: String,
  typeSymbol: XsTypeSymbol,
  defaultValue: Option[String],
  fixedValue: Option[String],  
  minOccurs: Int,
  maxOccurs: Int) extends Decl

object ElemDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    if (!(node \ "@ref").isEmpty) {
      val ref = (node \ "@ref").text.replaceFirst(config.myPrefix, "")
      
      Main.log("ElemDecl.fromXML: " + ref)
      if (!config.elems.contains(ref)) {
        throw new Exception("xsd: Element ref not found " + ref)
      }
      
      val that = config.elems(ref)
      val minOccurs = if ((node \ "@minOccurs").isEmpty)
        that.minOccurs
      else
        buildOccurrence((node \ "@minOccurs").text)
      val maxOccurs = if ((node \ "@maxOccurs").isEmpty)
        that.maxOccurs
      else
        buildOccurrence((node \ "@maxOccurs").text)
      
      ElemDecl(that.name, that.typeSymbol, that.defaultValue, that.fixedValue,
        minOccurs, maxOccurs)
    } else {
      val name = (node \ "@name").text
      Main.log("ElemDecl.fromXML: " + name)
      var typeSymbol: XsTypeSymbol = xsAny
      val typeName = (node \ "@type").text
      
      if (typeName != "") {
        typeSymbol = TypeSymbolParser.fromString(typeName, config)
      } else {
        for (child <- node.child) child match {
          case <complexType>{ _* }</complexType> =>
            typeSymbol = new ReferenceTypeSymbol(ComplexTypeDecl.buildName(child))

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
      val minOccurs = buildOccurrence((node \ "@minOccurs").text)
      val maxOccurs = buildOccurrence((node \ "@maxOccurs").text)
      
      val elem = ElemDecl(name, typeSymbol, defaultValue, fixedValue, minOccurs, maxOccurs)
      config.elems += (elem.name -> elem)
      elem   
    }
  }
  
  def buildOccurrence(value: String) =
    if (value == "")
      1
    else if (value == "unbounded")
      Integer.MAX_VALUE
    else
      value.toInt
}


abstract class TypeDecl extends Decl

case class ComplexTypeDecl(name: String,
  // derivedFrom: DerivSym,
  content: ContentDecl,
  attributes: List[AttributeDecl]) extends TypeDecl

object ComplexTypeDecl {  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    Main.log("ComplexTypeDecl.fromXML: " + node.toString)
    val name = buildName(node)
    var attributes: List[AttributeDecl] = Nil
    var derivedFrom = Restricts(xsAny)
    var content: ContentDecl = CompositorContentDecl(SequenceDecl(Nil))
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        throw new Exception("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        content = CompositorContentDecl(AllDecl.fromXML(child, config))
      case <choice>{ _* }</choice> =>
        content = CompositorContentDecl(ChoiceDecl.fromXML(child, config))
      case <sequence>{ _* }</sequence> =>
        content = CompositorContentDecl(SequenceDecl.fromXML(child, config))
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes
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
  
  def buildName(node: scala.xml.Node) = {
    val name = (node \ "@name").text
    if (name != "")
      name
    else
      "complexType@" + node.hashCode.toString
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

case class SimpleTypeDecl(name: String, content: ContentTypeDecl) extends TypeDecl {
  override def toString(): String = name + "(" + content.toString + ")"
}

object SimpleTypeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    Main.log("SimpleTypeDecl.fromXML: " + node.toString)
    
    val name = buildName(node)
    
    var content: ContentTypeDecl = null
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction>  => content = RestrictionDecl.fromXML(child, config) 
      case <list>{ _* }</list>                => content = ListDecl.fromXML(child, config)
      case <union>{ _* }</union>              => content = UnionDecl.fromXML(child, config)
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

abstract class ContentDecl extends Decl

case class CompositorContentDecl(compositor: HasParticle) extends ContentDecl

case class SimpleContentDecl(content: ContentTypeDecl) extends ContentDecl

object SimpleContentDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    var content: ContentTypeDecl = RestrictionDecl.empty
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = RestrictionDecl.fromXML(child, config)
      case <extension>{ _* }</extension> =>
        content = ExtensionDecl.fromXML(child, config)
      case _ =>
    }
       
    SimpleContentDecl(content)
  }
}

case class ComplexContentDecl(content: ContentTypeDecl) extends ContentDecl

object ComplexContentDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    var content: ContentTypeDecl = RestrictionDecl.empty
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = RestrictionDecl.fromXML(child, config)
      case <extension>{ _* }</extension> =>
        content = ExtensionDecl.fromXML(child, config)
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
    case <element>{ _* }</element>   => ElemDecl.fromXML(node, config)
    case <choice>{ _* }</choice>     => ChoiceDecl.fromXML(node, config)
    case <sequence>{ _* }</sequence> => SequenceDecl.fromXML(node, config)
    case _ => throw new Exception("xsd: Unspported content type " + node.label)   
  }
}

trait HasParticle {
  val particles: List[Decl]
}

case class SequenceDecl(particles: List[Decl]) extends CompositorDecl with HasParticle

object SequenceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    SequenceDecl(CompositorDecl.fromNodeSeq(node.child, config))
  }
}

case class ChoiceDecl(particles: List[Decl]) extends CompositorDecl with HasParticle

object ChoiceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    ChoiceDecl(CompositorDecl.fromNodeSeq(node.child, config))
  }
}

case class AllDecl(particles: List[Decl]) extends CompositorDecl with HasParticle

object AllDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    AllDecl(CompositorDecl.fromNodeSeq(node.child, config))
  }
}

abstract class ContentTypeDecl extends Decl

case class RestrictionDecl(base: XsTypeSymbol,
  compositor: HasParticle,
  attributes: List[AttributeDecl]) extends ContentTypeDecl

object RestrictionDecl {
  def empty =
    RestrictionDecl(xsAny, SequenceDecl(Nil), Nil)
  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: HasParticle = SequenceDecl(Nil)
    var attributes: List[AttributeDecl] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        throw new Exception("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        compositor = AllDecl.fromXML(child, config)
      case <choice>{ _* }</choice> =>
        compositor = ChoiceDecl.fromXML(child, config)
      case <sequence>{ _* }</sequence> =>
        compositor = SequenceDecl.fromXML(child, config)
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes
      
      case _ =>
    }
    
    RestrictionDecl(base, compositor, attributes.reverse)
  }
}

case class ExtensionDecl(base: XsTypeSymbol,
  compositor: HasParticle,
  attributes: List[AttributeDecl]) extends ContentTypeDecl

object ExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: HasParticle = SequenceDecl(Nil)
    var attributes: List[AttributeDecl] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        throw new Exception("Unsupported content type: " + child.toString)
      case <all>{ _* }</all> =>
        compositor = AllDecl.fromXML(child, config)
      case <choice>{ _* }</choice> =>
        compositor = ChoiceDecl.fromXML(child, config)
      case <sequence>{ _* }</sequence> =>
        compositor = SequenceDecl.fromXML(child, config)
      case <attribute>{ _* }</attribute> =>
        attributes = AttributeDecl.fromXML(child, config) :: attributes
      
      case _ =>
    }
       
    ExtensionDecl(base, compositor, attributes.reverse)
  }  
}

case class ListDecl() extends ContentTypeDecl

object ListDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    ListDecl()
  }
}

case class UnionDecl() extends ContentTypeDecl

object UnionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    UnionDecl()
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
