/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

abstract class ContentTypeDecl extends Decl

case class SimpContRestrictionDecl(base: XsTypeSymbol,
  attributes: List[AttributeDecl]) extends ContentTypeDecl

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
  attributes: List[AttributeDecl]) extends ContentTypeDecl

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
  attributes: List[AttributeDecl]) extends ContentTypeDecl
  
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
        throw new Exception("Unsupported content type: " + child.toString)
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
  attributes: List[AttributeDecl]) extends ContentTypeDecl

object CompContExtensionDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val baseName = (node \ "@base").text
    val base = TypeSymbolParser.fromString(baseName, config)
    var compositor: Option[HasParticle] = None
    var attributes: List[AttributeDecl] = Nil
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        throw new Exception("Unsupported content type: " + child.toString)
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
