/**
 * @author  e.e d3si9n
 */

import scalaxb._
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
  
  trait CustomXMLStandardTypes extends scalaxb.XMLStandardTypes {
    override implicit lazy val __IntXMLFormat: XMLFormat[Int] = new XMLFormat[Int] {
      def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, Int] = try { Right(seq.text.toInt + 1) }
        catch { case e: Exception => Left(e.toString) }

      def writes(obj: Int, namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        Helper.stringToXML((obj - 1).toString, namespace, elementLabel, scope)
    }

    override implicit lazy val __LongXMLFormat: XMLFormat[Long] = new XMLFormat[Long] {
      def reads(seq: scala.xml.NodeSeq, stack: List[scalaxb.ElemName]): Either[String, Long] = try { Right(seq.text.toLong + 1) }
        catch { case e: Exception => Left(e.toString) }

      def writes(obj: Long, namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        Helper.stringToXML((obj - 1).toString, namespace, elementLabel, scope)
    }
  }
  
  trait CustomXMLProtocol extends XMLProtocol {
    trait CustomGeneralSingularSimpleTypeTestFormat extends DefaultGeneralSingularSimpleTypeTestFormat {
      override def typeName: Option[String] = Some("SingularSimpleTypeTest")
      
      // hardcode to SKIM.
      override def parser(node: scala.xml.Node, stack: List[scalaxb.ElemName]): Parser[general.SingularSimpleTypeTest] =
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
        general.SingularSimpleTypeTest(fromXML[Long](p1, scalaxb.ElemName(node) :: stack),
          p2.nilOption map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) },
          p3.headOption map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) },
          p4.headOption map { _.nilOption map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) }},
          p5.toSeq map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) },
          p6.toSeq map { _.nilOption map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) }},
          SKIM,
          p8.nilOption map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) },
          p9.headOption map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) },
          p10.headOption map { _.nilOption map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) }},
          p11.toSeq map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) },
          p12.toSeq map { _.nilOption map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) }},
          (node \ "@attr1").headOption map { fromXML[Long](_, scalaxb.ElemName(node) :: stack) },
          (node \ "@attr2").headOption map { fromXML[general.MilkType](_, scalaxb.ElemName(node) :: stack) }) }
    }
  }

  def testSingularSimpleType {
    val customXMLProtocol = new CustomXMLProtocol with CustomXMLStandardTypes {}
    implicit val GeneralSingularSimpleTypeTestFormat = new customXMLProtocol.CustomGeneralSingularSimpleTypeTestFormat {}

    val defaultScope = scalaxb.toScope(None -> "http://www.example.com/general",
      Some("xs") -> "http://www.w3.org/2001/XMLSchema",
      Some("gen") -> "http://www.example.com/general",
      Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance")

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
