/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

import scala.xml.{TypeSymbol}
import scala.collection.{Map}
import schema2src.{Main}
import scala.collection.mutable
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

case class AnnotationDecl() extends Decl

object AnnotationDecl {
  def fromXML(node: scala.xml.Node) = AnnotationDecl() 
}

case class AttributeDecl(name: String, typeSymbol: SimpleTypeSymbol) extends Decl

object AttributeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val ref = (node \ "@ref").text
    if (ref != "") {
      Main.log("AttributeDelc.fromXML: " + ref)
      if (!config.attrs.contains(ref)) {
        throw new Exception("xsd: Attribute ref not found " + ref)
      }
      
      config.attrs(ref)
    } else {
      val name = (node \ "@name").text
      Main.log("AttributeDelc.fromXML: " + name)
      var typeSymbol: SimpleTypeSymbol = xsUnknown
      val typeName = (node \ "@type").text
      if (typeName != "") {
        typeSymbol = TypeSymbolParser.toSimpleTypeSymbol(typeName, config)
      } else {
        for (child <- node.child) child match {
          case <simpleType>{ _* }</simpleType> =>
            typeSymbol = new SimpleTypeSymbol(SimpleTypeDecl.buildName(child))
          case _ =>
        }
      } // if-else
      
      val attr = AttributeDecl(name, typeSymbol)
      config.attrs += (attr.name -> attr)
      attr   
    }
  } 
}

case class ElemDecl(name: String,
  typeSymbol: XsTypeSymbol,
  minOccurs: Int,
  maxOccurs: Int) extends Decl

object ElemDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {    
    val ref = (node \ "@ref").text
    if (ref != "") {
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
      
      ElemDecl(that.name, that.typeSymbol, minOccurs, maxOccurs)
    } else {
      val name = (node \ "@name").text
      Main.log("ElemDecl.fromXML: " + name)
      var typeSymbol: XsTypeSymbol = xsdAny
      val typeName = (node \ "@type").text
      
      if (typeName != "") {
        typeSymbol = TypeSymbolParser.fromString(typeName, config)
      } else {
        for (child <- node.child) child match {
          case <complexType>{ _* }</complexType> =>
            typeSymbol = new ComplexTypeSymbol(ComplexTypeDecl.buildName(child))

          case <simpleType>{ _* }</simpleType> =>
            typeSymbol = new SimpleTypeSymbol(SimpleTypeDecl.buildName(child))
                        
          case _ =>
        }
      } // if-else
      
      val minOccurs = buildOccurrence((node \ "@minOccurs").text)
      val maxOccurs = buildOccurrence((node \ "@maxOccurs").text)
      val elem = ElemDecl(name, typeSymbol, minOccurs, maxOccurs)
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

abstract class TypeDecl extends Decl

case class ComplexTypeDecl(name: String,
  derivedFrom: DerivSym,
  compositor: HasParticle,
  attributes: Array[AttributeDecl]) extends TypeDecl

object ComplexTypeDecl {  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    import scala.collection.mutable.ArrayBuilder
    
    Main.log("ComplexTypeDecl.fromXML: " + node.toString)
    val name = buildName(node)
    var compositor: HasParticle = null
    val attributes = ArrayBuilder.make[AttributeDecl]
    
    for (child <- node.child) child match {
      case <all>{ _* }</all>           => compositor = AllDecl.fromXML(child, config) 
      case <choice>{ _* }</choice>     => compositor = ChoiceDecl.fromXML(child, config)
      case <sequence>{ _* }</sequence> => compositor = SequenceDecl.fromXML(child, config)
      case <attribute>{ _* }</attribute> => attributes += AttributeDecl.fromXML(child, config)
      case _ =>     
    }
    
    // val contentModel = ContentModel.fromSchema(firstChild(node))
    val typ = ComplexTypeDecl(name, null, compositor, attributes.result)
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

abstract class SimpleTypeContent

case class RestrictionDecl(base: XsTypeSymbol) extends SimpleTypeContent

object RestrictionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    RestrictionDecl(base)
  }
}

case class ListDecl() extends SimpleTypeContent

object ListDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    ListDecl()
  }
}

case class UnionDecl() extends SimpleTypeContent

object UnionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    UnionDecl()
  }
}

case class SimpleTypeDecl(name: String, content: SimpleTypeContent) extends TypeDecl {
  override def toString(): String = name + "(" + content.toString + ")"
}

object SimpleTypeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    Main.log("SimpleTypeDecl.fromXML: " + node.toString)
    
    val name = buildName(node)
    
    var content: SimpleTypeContent = null
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

case class SchemaDecl(elems: Map[String, ElemDecl], types: Map[String, TypeDecl]) {
  override def toString(): String = {
    "SchemaDecl(" + elems.valuesIterator.mkString(",") + "," +
      types.valuesIterator.mkString(",")  + ")"
  }
}

object SchemaDecl {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig = DefaultParserConfig) = {
    val elems = mutable.Map.empty[String, ElemDecl]
    val types = mutable.Map.empty[String, TypeDecl]
    val attrs = mutable.Map.empty[String, AttributeDecl]
    config.elems = elems
    config.types = types
    config.attrs = attrs
    
    val children = node match {
      case <schema>{ children @ _* }</schema> =>
        Main.log("SchemaDecl: schema tag found")
        for (child <- children; if child.isInstanceOf[scala.xml.Elem])
          yield child
      case _ => throw new Exception("XSD Validation error:" + node.toString)
    }
    
    for (child <- children) child match {
      case <element>{ _* }</element> => ElemDecl.fromXML(child, config)
      case _ =>
    }
    
    for (complexType <- children \\ "complexType")
      ComplexTypeDecl.fromXML(complexType, config)
    
    for (simpleType <- children \\ "simpleType")
      SimpleTypeDecl.fromXML(simpleType, config)
        
    for (elemPair <- elems) {
      val typeSymbol = elemPair._2.typeSymbol
      typeSymbol match {
        case symbol: BuiltInSimpleTypeSymbol =>
        case symbol: ComplexTypeSymbol =>
          if (!types.contains(symbol.name))
            throw new Exception("SchemaDecl: type not found " + elemPair._1 + ": " + symbol.name)
          types(symbol.name) match {
            case decl: ComplexTypeDecl => symbol.decl = decl
            case _ => throw new Exception("SchemaDecl: type does not match ")
          }
          
        case symbol: SimpleTypeSymbol => 
          if (!types.contains(symbol.name))
            throw new Exception("SchemaDecl: type not found " + elemPair._1 + ": " + symbol.name)
          types(symbol.name) match {
            case decl: SimpleTypeDecl => symbol.decl = decl
            case _ => throw new Exception("SchemaDecl: type does not match ")
          }
          
        case symbol: ReferenceTypeSymbol =>
          if (!types.contains(symbol.name))
            throw new Exception("SchemaDecl: type not found " + elemPair._1 + ": " + symbol.name)
          symbol.decl = types(symbol.name)          
      }
      
      for (attrPair <- attrs) {
        val typeSymbol = attrPair._2.typeSymbol
        typeSymbol match {
          case symbol: BuiltInSimpleTypeSymbol => 
          case symbol: SimpleTypeSymbol        =>
            if (!types.contains(symbol.name))
              throw new Exception("SchemaDecl: type not found " + attrPair._1 + ": " + symbol.name)
            types(symbol.name) match {
              case decl: SimpleTypeDecl => symbol.decl = decl
              case _ => throw new Exception("SchemaDecl: type does not match ")
            }       
        }    
      }
    }
    
    SchemaDecl(immutable.Map.empty[String, ElemDecl] ++ elems,
      immutable.Map.empty[String, TypeDecl] ++ types)
  }
}

object TypeSymbolParser {
  def toSimpleTypeSymbol(name: String, config: ParserConfig): SimpleTypeSymbol = {
    val xsType = XsTypeSymbol.toTypeSymbol(name.replaceFirst(config.xsPrefix, ""))
    if (xsType != xsUnknown) {
      xsType
    } else {
      val n = name.replaceFirst(config.myPrefix, "")
      new SimpleTypeSymbol(n)
    }
  }
  
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
