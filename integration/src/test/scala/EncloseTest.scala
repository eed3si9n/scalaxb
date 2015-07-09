import scalaxb.compiler.{Config}

object EncloseTest extends TestBase {
  lazy val generated = module.processNode(dupeChildTypes,
    new Config(packageNames = Map(None -> Some("enclose")),
      packageDir = true, outdir = tmp, prependFamilyName = true))

  "enclose.scala file" in {
    println(generated(0))
    generated(0) must not contain "Apple2"
  }

  val dupeChildTypes = <xs:schema targetNamespace="http://www.example.com/mixed"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns="http://www.example.com/enclose"
  elementFormDefault="qualified">
  <xs:element name="element1">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="apple">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="s1" type="xs:string" />
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element name="element2">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="apple">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="s1" type="xs:string" />
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
}