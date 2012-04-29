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

  val sequenceXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SequenceComplexTypeTest">
        <xs:sequence>
          <xs:sequence>
            <xs:element name="person1" type="gen:Person"/>
            <xs:element name="address1" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence>
            <xs:element name="person2" nillable="true" type="gen:Person"/>
            <xs:element name="address2" nillable="true" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence minOccurs="0">
            <xs:element name="person3" type="gen:Person"/>
            <xs:element name="address3" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence minOccurs="0">
            <xs:element name="person4" nillable="true" type="gen:Person"/>
            <xs:element name="address4" nillable="true" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence maxOccurs="unbounded">
            <xs:element name="person5" type="gen:Person"/>
            <xs:element name="address5" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence maxOccurs="unbounded">
            <xs:element name="person6" nillable="true" type="gen:Person"/>
            <xs:element name="address6" nillable="true" type="gen:Address"/>
          </xs:sequence>
          <xs:sequence>
            <xs:element name="int1" type="xs:int"/>
            <xs:element name="int2" type="xs:int"/>
          </xs:sequence>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="EmptySequenceComplexTypeTest">
        <xs:sequence/>
      </xs:complexType>

      <xs:complexType name="LongSequenceComplexTypeTest">
        <xs:sequence>
          <xs:element name="int1" type="xs:int"/>
          <xs:element name="int2" type="xs:int"/>
          <xs:element name="int3" type="xs:int"/>
          <xs:element name="int4" type="xs:int"/>
          <xs:element name="int5" type="xs:int"/>
          <xs:element name="int6" type="xs:int"/>
          <xs:element name="int7" type="xs:int"/>
          <xs:element name="int8" type="xs:int"/>
          <xs:element name="int9" type="xs:int"/>
          <xs:element name="int10" type="xs:int"/>
          <xs:element name="int11" type="xs:int"/>
          <xs:element name="int12" type="xs:int"/>
          <xs:element name="int13" type="xs:int"/>
          <xs:element name="int14" type="xs:int"/>
          <xs:element name="int15" type="xs:int"/>
          <xs:element name="int16" type="xs:int"/>
          <xs:element name="int17" type="xs:int"/>
          <xs:element name="int18" type="xs:int"/>
          <xs:element name="int19" type="xs:int"/>
          <xs:element name="int20" type="xs:int"/>
          <xs:element name="int21" type="xs:int"/>
          <xs:element name="int22" type="xs:int"/>
          <xs:element name="int23" type="xs:int"/>
          <xs:element name="int24" type="xs:int"/>
          <xs:element name="int25" type="xs:int"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="MultipleSequenceComplexTypeTest">
        <xs:sequence minOccurs="0" maxOccurs="unbounded">
          <xs:element name="int1" type="xs:int"/>
          <xs:element name="int2" type="xs:int"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

  val choiceXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="ChoiceComplexTypeTest">
        <xs:sequence>
          <xs:choice>
            <xs:element name="person1" type="gen:Person"/>
            <xs:element name="address1" type="gen:Address"/>
          </xs:choice>
          <xs:choice>
            <xs:element name="person2" nillable="true" type="gen:Person"/>
            <xs:element name="address2" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice minOccurs="0">
            <xs:element name="person3" type="gen:Person"/>
            <xs:element name="address3" type="gen:Address"/>
          </xs:choice>
          <xs:choice minOccurs="0">
            <xs:element name="person4" nillable="true" type="gen:Person"/>
            <xs:element name="address4" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice maxOccurs="unbounded">
            <xs:element name="person5" type="gen:Person"/>
            <xs:element name="address5" type="gen:Address"/>
          </xs:choice>
          <xs:choice maxOccurs="unbounded">
            <xs:element name="person6" nillable="true" type="gen:Person"/>
            <xs:element name="address6" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice>
            <xs:element name="int1" type="xs:int"/>
            <xs:element name="int2" type="xs:int"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>
  
  val allXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="AllComplexTypeTest">
        <xs:all>
          <xs:element name="address1" type="gen:Address"/>
          <xs:element name="address2" minOccurs="0" type="gen:Address"/>
          <xs:element name="string3" type="xs:string"/>
          <xs:element name="string4" type="xs:string"/>
          <xs:element name="string5" type="xs:string"/>
          <xs:element name="string6" type="xs:string"/>
        </xs:all>
      </xs:complexType>

      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

  val wildcardXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="WildcardTest">
        <xs:sequence>
          <xs:element name="person1" type="gen:Person"/>
          <xs:any namespace="##other" processContents="lax"/>
          <xs:any namespace="##local" processContents="lax"/>
          <xs:any namespace="##targetNamespace" processContents="lax"/>
          <xs:any namespace="http://www.example.com/foo" processContents="lax"/>
          <xs:choice>
            <xs:element name="person2" nillable="true" type="gen:Person"/>
            <xs:any namespace="##other" processContents="lax"/>
          </xs:choice>
          <xs:element name="person3" minOccurs="0" type="gen:Person"/>
          <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
          <xs:choice>
            <xs:element name="person4" minOccurs="0" nillable="true" type="gen:Person"/>
            <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
          </xs:choice>
          <xs:any namespace="##other" processContents="lax" maxOccurs="unbounded"/>
          <xs:element name="person5" maxOccurs="unbounded" type="gen:Person"/>
          <xs:any />
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

  val seqParamXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SeqParamTest">
        <xs:sequence>
          <xs:element name="foo" type="xs:string" maxOccurs="unbounded" />
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="NillableSeqParamTest">
        <xs:sequence>
          <xs:element name="foo" type="xs:string" maxOccurs="unbounded" nillable="true" />
        </xs:sequence>
      </xs:complexType>
    </xs:schema>   
}
