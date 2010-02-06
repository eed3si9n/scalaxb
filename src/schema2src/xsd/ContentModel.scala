/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

import scala.xml._
import scala.util.regexp.WordExp
import scala.util.automata._

object ContentModel extends WordExp  {

  type _labelT = ElemRef;
  type _regexpT = RegExp;

  object Translator extends WordBerrySethi  {
    override val lang: ContentModel.this.type = ContentModel.this;
    import lang._ ;
  }

  case class ElemRef(name: String = "") extends Label {
    override def toString() = name
  }
  
  def fromSchema(nodes: Seq[Node]): Seq[RegExp] = 
    for (child <- nodes; if child.isInstanceOf[scala.xml.Elem])
      yield fromSchema(child)

  def fromSchema(node: Node): RegExp = node match {
    case <sequence>{ children @ _* }</sequence> => Sequ(fromSchema(filterElem(children)):_*);
    case <choice>{ children @ _* }</choice> =>  Alt(fromSchema(filterElem(children)):_*);
    case <group>{ children @ _* }</group> => Sequ(fromSchema(filterElem(children)):_*);
    case <element>{ children @ _* }</element>   =>
      if ((node \ "@name").text != "")
        Letter(ElemRef((node \ "@name").text))
      else
        Letter(ElemRef((node \ "@ref").text))
    case <complexContent>{ children @ _* }</complexContent>  => Letter(ElemRef())
    case <simpleContent>{ children @ _* }</simpleContent>  => Letter(ElemRef())
    case <extension>{ children @ _* }</extension>  => Letter(ElemRef())
    case _ => throw new Exception("XSD ContentModel error:" + node.toString)
  }
  
  def filterElem(nodes: Seq[Node]) = {
    for (child <- nodes; if child.isInstanceOf[scala.xml.Elem])
      yield child  
  }
}

sealed abstract class ContentModel ;
case class ELEMENTS(r:ContentModel.RegExp) extends ContentModel ;
case class MIXED(r:ContentModel.RegExp) extends ContentModel ;
case object SimpleContent extends ContentModel ;
