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
  
  trait CustomXMLProtocol extends DefaultXMLProtocol {
    override def buildGeneralSingularSimpleTypeTestFormat = new CustomGeneralSingularSimpleTypeTestFormat {}
    trait CustomGeneralSingularSimpleTypeTestFormat extends DefaultGeneralSingularSimpleTypeTestFormat {
      override def typeName: Option[String] = Some("SingularSimpleTypeTest")
      
      // hardcode to SKIM.
      override def parser(node: scala.xml.Node): Parser[SingularSimpleTypeTest] =
        (scalaxb.ElemName(targetNamespace, "number1")) ~ 
        (scalaxb.ElemName(targetNamespace, "number2")) ~ 
        opt(scalaxb.ElemName(targetNamespace, "number3")) ~ 
        opt(scalaxb.ElemName(targetNamespace, "number4")) ~ 
        rep(scalaxb.ElemName(targetNamespace, "number5")) ~ 
        rep(scalaxb.ElemName(targetNamespace, "number6")) ~ 
        (scalaxb.ElemName(targetNamespace, "milk1")) ~ 
        (scalaxb.ElemName(targetNamespace, "milk2")) ~ 
        opt(scalaxb.ElemName(targetNamespace, "milk3")) ~ 
        opt(scalaxb.ElemName(targetNamespace, "milk4")) ~ 
        rep(scalaxb.ElemName(targetNamespace, "milk5")) ~ 
        rep(scalaxb.ElemName(targetNamespace, "milk6")) ^^
        { case p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8 ~ p9 ~ p10 ~ p11 ~ p12 =>
        SingularSimpleTypeTest(fromXML[Long](p1),
          p2.nilOption map { fromXML[Long](_) },
          p3.headOption map { fromXML[Long](_) },
          p4.headOption map { _.nilOption map { fromXML[Long](_) }},
          p5.toSeq map { fromXML[Long](_) },
          p6.toSeq map { _.nilOption map { fromXML[Long](_) }},
          SKIM,
          p8.nilOption map { fromXML[MilkType](_) },
          p9.headOption map { fromXML[MilkType](_) },
          p10.headOption map { _.nilOption map { fromXML[MilkType](_) }},
          p11.toSeq map { fromXML[MilkType](_) },
          p12.toSeq map { _.nilOption map { fromXML[MilkType](_) }},
          (node \ "@attr1").headOption map { fromXML[Long](_) },
          (node \ "@attr2").headOption map { fromXML[MilkType](_) }) }
    }
  }
  
  object CustomXMLProtocol extends CustomXMLProtocol with CustomXMLStandardTypes {
    import scalaxb.Scalaxb._
    val defaultScope = toScope(None -> "http://www.example.com/general",
      Some("xs") -> "http://www.w3.org/2001/XMLSchema",
      Some("gen") -> "http://www.example.com/general",
      Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance")    
  }
  
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
          SKIM, None, None, None, Seq(WHOLE, SKIM), Seq(),
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
