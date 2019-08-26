/**
 * @author  e.e d3si9n
 */

import scalaxb._
import org.w3.xmldsig._
import org.w3.xmlenc._
import org.xml.saml2.assertion._
import org.xml.saml2.metadata._
import java.net.URI

object SamlUsage {
  def main(args: Array[String]): Unit = {
    allTests
  }

  def allTests = {
    testAttribute
    true
  }
  
  // case class AttributeType(AttributeValue: Seq[scalaxb.DataRecord[Option[Any]]] = Nil,
  //   attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends AttributeTypable {
  //   lazy val Name = attributes("@Name").as[String]
  //   lazy val NameFormat = attributes.get("@NameFormat") map { _.as[java.net.URI] }
  //   lazy val FriendlyName = attributes.get("@FriendlyName") map { _.as[String] }
  // }
  def testAttribute = {
    val subject = <saml:Attribute
      xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:x500="urn:oasis:names:tc:SAML:2.0:profiles:attribute:X500"
      x500:Encoding="LDAP"
      NameFormat="urn:oasis:names:tc:SAML:2.0:attrname-format:uri"
      Name="urn:oid:1.3.6.1.4.1.5923.1.1.1.1"
      FriendlyName="eduPersonAffiliation">
      <AttributeValue xsi:type="xs:string">member</AttributeValue>
      <AttributeValue xsi:type="xs:string">staff</AttributeValue>
    </saml:Attribute>
    
    val obj = fromXML[AttributeType](subject)
    obj match {
      case x@AttributeType(Seq(y@DataRecord(_, _, _), z@DataRecord(_, _, _) ), _)
        if (x.Name == "urn:oid:1.3.6.1.4.1.5923.1.1.1.1") &&
          (x.NameFormat == Some(new URI("urn:oasis:names:tc:SAML:2.0:attrname-format:uri"))) &&
          (x.FriendlyName == Some("eduPersonAffiliation")) => 
      case _ => sys.error("match failed: " + obj.toString)
    }
    
    println(obj.toString)
  }
}
