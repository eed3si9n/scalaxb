/**
 * @author  e.e d3si9n
 */

import scalaxb._
import Scalaxb._
import general._

object CustomizationUsage {
  val NS = Some("http://www.example.com/general")
  
  def main(args: Array[String]) = {
    allTests
  }
  
  def allTests = {
    testSingularSimpleType
    true
  }
  
  trait CustomXMLStandardTypes extends scalaxb.DefaultXMLStandardTypes {
    override def __buildIntXMLFormat: XMLFormat[Int] = new XMLFormat[Int] {
      def reads(seq: scala.xml.NodeSeq): Either[String, Int] = try { Right(seq.text.toInt + 1) }
        catch { case e: Exception => Left(e.toString) }

      def writes(obj: Int, namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        Helper.stringToXML((obj - 1).toString, namespace, elementLabel, scope)
    }
    
    override def __buildLongXMLFormat: XMLFormat[Long] = new XMLFormat[Long] {
      def reads(seq: scala.xml.NodeSeq): Either[String, Long] = try { Right(seq.text.toLong + 1) }
        catch { case e: Exception => Left(e.toString) }

      def writes(obj: Long, namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        Helper.stringToXML((obj - 1).toString, namespace, elementLabel, scope)
    }
  }
  
  object CustomXMLProtocol extends general.DefaultXMLProtocol with CustomXMLStandardTypes
  
  def testSingularSimpleType {
    import CustomXMLProtocol._
    
    val subject0 = <foo>1</foo>
    val obj0 = fromXML[Int](subject0)
    println(obj0)
    println(toXML[Int](obj0, "foo", defaultScope))
    
    val subject = <foo xmlns="http://www.example.com/general"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <number1>1</number1>
      <number2 xsi:nil="true"/>
      <number4>1</number4>
      <number5>2</number5><number5>1</number5>
      
      <milk1>WHOLE</milk1>
      <milk2 xsi:nil="true"/>
      <milk5>WHOLE</milk5><milk5>SKIM</milk5>
    </foo>
    val obj = fromXML[SingularSimpleTypeTest](subject)
    
    def check(obj: Any) = obj match {
        case SingularSimpleTypeTest(2, None, None, Some(Some(2)), Seq(3, 2), Seq(), 
          WHOLE, None, None, None, Seq(WHOLE, SKIM), Seq(),
          None, None) =>
        case _ => error("match failed: " + obj.toString)
      }
    println(obj)
    check(obj)
    val document = toXML[SingularSimpleTypeTest](obj, "foo", defaultScope)
      check(fromXML[SingularSimpleTypeTest](document))
    println(document)
  }
}
