object Example {
  val complexTypeCardinalityXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="SingularComplexTypeTest">
        <xs:sequence>
          <xs:element name="person1" type="gen:Person"/>
          <xs:element name="person2" nillable="true" type="gen:Person"/>
          <xs:element name="person3" minOccurs="0" type="gen:Person"/>
          <xs:element name="person4" minOccurs="0" nillable="true" type="gen:Person"/>
          <xs:element name="person5" maxOccurs="unbounded" type="gen:Person"/>
          <xs:element name="person6" maxOccurs="unbounded" nillable="true" type="gen:Person"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>
}
