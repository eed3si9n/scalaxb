package scalaxb.specs

import org.specs2._

object EntitySpec_3 extends Specification { def is =                          s2"""
  this is a specification to check the generated entity source
  
  restrictions of a built-in should
    be referenced as the corresponding built-in type                          $restriction1
  
  restrictions of simple type should
    be referenced as its base built-in type                                   $restriction2
  
  lists of a simple type should
    be referenced as Seq of its base type                                     $derivation1
  
  complex derivation of a simple type should
    generate a case class named similarly with a parameter named value        $derivation2
  
  complex derivation of a complex type with simple content should
    generate a case class named similarly with a parameter named value        $derivation3
  
  complex derivation of a complex type with complex content should
    generate a trait which the case classes extend                            $derivation4
  
  complex derivation of a complex type from another schema should
    generate a trait which the case classes extend                            $derivation5"""

  import Example._
  // scalaxb.compiler.Module.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver 

  def restriction1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="quantity">
            <xs:simpleType>
              <xs:restriction base="xs:positiveInteger">
                <xs:maxExclusive value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""quantity: BigInt""")
  }

  def restriction2 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:simpleType name="ShortString">
        <xs:restriction base="xs:string">
          <xs:maxLength value="140"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="comment">
            <xs:simpleType>
              <xs:restriction base="gen:ShortString">
                <xs:maxLength value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="comment2">
            <xs:simpleType>
              <xs:restriction>
                <xs:simpleType>
                  <xs:restriction base="gen:ShortString">
                    <xs:maxLength value="130"/>
                  </xs:restriction>
                </xs:simpleType>
                <xs:maxLength value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""comment: String""") and contain("""comment2: String""")
  }

  lazy val derivEntitySource = module.processNode(derivationXML, "example")(0)

  def derivation1 = {
    println(derivEntitySource)
    derivEntitySource must contain("""case class SimpleTypeTest(numberlist1: Seq[Long], milklist1: Seq[example.MilkType])""")
  }

  def derivation2 = {
    (derivEntitySource must contain("""case class ComplexMilk(value: example.MilkType, attributes: Map[String, scalaxb.DataRecord[Any]])""")) and
    (derivEntitySource must contain("""case class ComplexListOfMilk(value: Seq[example.MilkType], attributes: Map[String, scalaxb.DataRecord[Any]])"""))
  }

  def derivation3 = {
    derivEntitySource must contain("""case class ComplexMilkDerived(value: example.MilkType, attributes: Map[String, scalaxb.DataRecord[Any]])""")
  }

  lazy val addressEntitySource = module.processNode(addressXML, "example")(0)

  def derivation4 = {
    println(addressEntitySource)
    (addressEntitySource.lines.toList must contain(allOf(
      """trait Addressable {""",
      """  def street: String""",
      """}"""))) and
    (addressEntitySource.lines.toList must contain(
      """case class Address(street: String, city: String, state: String) extends example.Addressable""")) and
    (addressEntitySource.lines.toList must contain(
      """case class USAddress(street: String, city: String, state: String, zip: BigInt, attributes: Map[String, scalaxb.DataRecord[Any]]) extends example.Addressable {"""))
  }

  lazy val importSources = module.processNodes(Seq(addressXML, importXML),
    scalaxb.compiler.Config(packageNames = Map(None -> Some("example"))))

  def derivation5 = {
    println(importSources(0))
    println(importSources(1))
    
    (importSources(0).lines.toList must contain(allOf(
      """trait Arrayable {""",
      """  def arrayGroup: Option[example.ArrayGroup]""",
      """}"""))) and
    (importSources(1).lines.toList must contain(
      """case class RestrictedArray(arrayGroup: Option[example.ArrayGroup]) extends example.Arrayable"""))
  }
}
