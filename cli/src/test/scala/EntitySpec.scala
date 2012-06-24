package scalaxb.specs

import org.specs2._

object EntitySpec extends Specification { def is = sequential                 ^
  "this is a specification to check the generated entity source"              ^
                                                                              p^
  "the generated entity source should"                                        ^
    "start with // Generated by"                                              ! entity1^
    "produce package mapped to the target namespace"                          ! entity2^
                                                                              end^
  "restrictions of a built-in should"                                         ^
    "be referenced as the corresponding built-in type"                        ! restriction1^
                                                                              end^
  "restrictions of simple type should"                                        ^
    "be referenced as its base built-in type"                                 ! restriction2^
                                                                              end^
  "lists of a simple type should"                                             ^
    "be referenced as Seq of its base type"                                   ! derivation1^
                                                                              end^
  "unions of simple types should"                                             ^
    "be referenced as String"                                                 ! union1^
                                                                              end^
  "top-level simple types with enumeration should"                            ^
    "generate a trait named similarly"                                        ! enum1^
    "each enumerations represented as case object"                            ! enum2^
    "be referenced as the trait"                                              ! enum3^
                                                                              end^
  "top-level complex types should"                                            ^
    "generate a case class named similarly"                                   ! complexType1^
    "not generate case class for the primary sequence"                        ! complexType2^
    "be referenced as the class/trait"                                        ! complexType3^
    "be referenced as Option[A] if nillable"                                  ! complexType3^
    "be referenced as Option[A] if optional"                                  ! complexType3^
    "be referenced as Option[Option[A]] if nillable and optional"             ! complexType3^
    "be referenced as Seq[A] if maxOccurs >1"                                 ! complexType3^
    "be referenced as Seq[Option[A]] if nillable and maxOccurs >1"            ! complexType3^
                                                                              end^
  "top-level elements with a local complex type should"                       ^
    "generate a case class named similarly"                                   ! element1^
                                                                              end^
  "local elements with a local complex type should"                           ^
    "generate a case class named similarly"                                   ! localelement1^
                                                                              end^
  "top-level named group should"                                              ^
    "generate a case class FooGroup out of the primary compositor"            ! group1^
                                                                              end

  import Example._
  // scalaxb.compiler.Module.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver
  lazy val emptyEntitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" />, "example1")(0)

  def entity1 = {
    println(emptyEntitySource)
    emptyEntitySource must startWith("// Generated by")
  }

  def entity2 = {
    emptyEntitySource must find("""package example1""".stripMargin)
  }

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

  def derivation1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:tns="http://www.example.com">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="numberlist1" type="tns:ListOfUnsignedInt"/>
          <xs:element name="milklist1" type="tns:ListOfMilk"/>
        </xs:sequence>
      </xs:complexType>

      <xs:simpleType name="ListOfUnsignedInt">
        <xs:list itemType="xs:unsignedInt"/>
      </xs:simpleType>

      <xs:simpleType name="ListOfMilk">
        <xs:list itemType="tns:MilkType"/>
      </xs:simpleType>

      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""numberlist1: Seq[Long], milklist1: Seq[example.MilkType]""")
  }

  def union1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:tns="http://www.example.com/">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="union">
            <xs:simpleType>
              <xs:union memberTypes="xs:string xs:int" />
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""union: String""")
  }

  lazy val enumEntitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
      xmlns:tns="http://www.example.com/"
      xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:simpleType name="MilkType">
      <xs:restriction base="xs:NMTOKEN">
        <xs:enumeration value="WHOLE"/>
        <xs:enumeration value="SKIM"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:complexType name="SimpleTypeTest">
      <xs:sequence>
        <xs:element name="milk1" type="tns:MilkType"/>
      </xs:sequence>
    </xs:complexType>
  </xs:schema>, "example")(0)

  def enum1 = {
    println(enumEntitySource)
    enumEntitySource must contain("""trait MilkType""")
  }

  def enum2 = {
    enumEntitySource must contain("""case object SKIM""")
  }

  def enum3 = {
    enumEntitySource must contain("""milk1: example.MilkType""")
  }

  lazy val complexTypeEntitySource = module.processNode(complexTypeCardinalityXML, "example")(0)

  lazy val expectedComplexTypeTest =
    """case class SingularComplexTypeTest(person1: example.Person, person2: Option[example.Person], """ +
      """person3: Option[example.Person], person4: Option[Option[example.Person]], """ +
      """person5: Seq[example.Person], person6: Seq[Option[example.Person]])"""

  def complexType1 = {
    println(complexTypeEntitySource)
    complexTypeEntitySource must contain("""case class Address(""")
  }

  def complexType2 = {
    complexTypeEntitySource must not contain("""AddressSequence""")
  }

  def complexType3 = {
    complexTypeEntitySource must contain(expectedComplexTypeTest)
  }

  def element1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:element name="topLevelElementTest">
        <xs:complexType>
          <xs:sequence>
            <xs:choice maxOccurs="unbounded">
              <xs:element name="foo" type="xs:string"/>
              <xs:any namespace="##other" processContents="lax" />
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""case class TopLevelElementTest(""")
  }

  def localelement1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/ipo"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:ipo="http://www.example.com/ipo">
      <xs:element name="comment" type="xs:string"/>

      <xs:complexType name="Items">
        <xs:sequence>
          <xs:element name="item" minOccurs="0" maxOccurs="unbounded">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="productName" type="xs:string"/>
                <xs:element name="quantity">
                  <xs:simpleType>
                    <xs:restriction base="xs:positiveInteger">
                      <xs:maxExclusive value="100"/>
                    </xs:restriction>
                  </xs:simpleType>
                </xs:element>
                <xs:element name="USPrice"    type="xs:decimal"/>
                <xs:element ref="ipo:comment" minOccurs="0"/>
                <xs:element name="shipDate"   type="xs:date" minOccurs="0"/>
              </xs:sequence>
              <xs:attribute name="partNum" type="xs:int" use="required"/>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    println(entitySource)
    (entitySource must contain("""case class Items(item: example.Item*)""")) and
    (entitySource must contain("""case class Item("""))
  }

  def group1 = {
    val entitySource = module.processNode(namedGroupXML, "example")(0)

    println(entitySource)
    (entitySource must not contain("""case class EmptySeqGroup""")) and
    (entitySource must contain("""case class SeqGroup(city: String)"""))    
  }
}
