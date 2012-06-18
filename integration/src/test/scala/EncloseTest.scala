package scalaxb.specs

import scalaxb.compiler.{Config}
import scalaxb.compiler.ConfigEntry._

object EncloseTest extends TestBase {
  val config = Config.default.update(PackageNames(Map(None -> Some("enclose")))).
    update(Outdir(tmp)).
    update(GeneratePackageDir).
    update(PrependFamilyName)

  lazy val generated = module.processNode(dupeChildTypes, config)

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