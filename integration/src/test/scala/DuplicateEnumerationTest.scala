import scalaxb.compiler.Config

object DuplicateEnumerationTest extends TestBase {
  val config = new Config(
    packageNames = Map(None -> Some("dupenum")),
    packageDir = true,
    outdir = tmp,
    prependFamilyName = true
  )
  lazy val generated = module.processNode(dupeEnums, config)

  "dupenum.scala file should compile" in {
    """case "N" => NValue""".r.findAllIn(generated(0)).size must beEqualTo(1)
    "case object NValue".r.findAllIn(generated(0)).size must beEqualTo(1)
  }

  val dupeEnums = <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="Root" type="RootType"></xs:element>
  <xs:complexType name="RootType">
    <xs:sequence>
      <xs:element name="Reason" type="ReasonType"></xs:element>
    </xs:sequence>
  </xs:complexType>
  <xs:simpleType name="ReasonType">
    <xs:restriction base="xs:token">
      <xs:enumeration value="H"></xs:enumeration>
      <xs:enumeration value="N"></xs:enumeration>
      <xs:enumeration value="U"></xs:enumeration>
      <xs:enumeration value="N"></xs:enumeration>
      <xs:enumeration value=""></xs:enumeration>
    </xs:restriction>
  </xs:simpleType>
</xs:schema>
}
