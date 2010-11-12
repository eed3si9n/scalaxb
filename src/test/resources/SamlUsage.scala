/**
 * @author  e.e d3si9n
 */

import scalaxb._
import Scalaxb._
import org.w3.xmldsig._
import org.w3.xmlenc._
import org.xml.saml2.assertion._
import org.xml.saml2.metadata._
import org.xml.saml2.assertion.DefaultXMLProtocol._

object SamlUsage {
  def main(args: Array[String]) = {
    allTests
  }

  def allTests = {
    testAttribute
    true
  }
  
  def testAttribute {
    val subject = <saml:Attribute
      xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:x500="urn:oasis:names:tc:SAML:2.0:profiles:attribute:X500"
      x500:Encoding="LDAP"
      NameFormat="urn:oasis:names:tc:SAML:2.0:attrname-format:uri"
      Name="urn:oid:1.3.6.1.4.1.5923.1.1.1.1"
      FriendlyName="eduPersonAffiliation">
      <saml:AttributeValue xsi:type="xs:string">member</saml:AttributeValue>
      <saml:AttributeValue xsi:type="xs:string">staff</saml:AttributeValue>
    </saml:Attribute>
    
    val obj = fromXML[AttributeType](subject)
    obj match {
      case AttributeType(Seq(x@DataRecord(_, _, _),
          y@DataRecord(_, _, _) ),
        "urn:oid:1.3.6.1.4.1.5923.1.1.1.1",
        Some(_),
        Some("eduPersonAffiliation"),
        Seq(z@DataRecord(_, _, _))) => 
      case _ => error("match failed: " + obj.toString)
    }
    
    println(obj.toString)
  }
}
