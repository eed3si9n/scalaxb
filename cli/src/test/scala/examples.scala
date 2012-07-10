package scalaxb.specs

object Example {
  val anyTypeXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="AnyTypeTest">
        <xs:sequence>
          <xs:element name="any1" type="xs:anyType"/>
          <xs:element name="any2" nillable="true" type="xs:anyType"/>
          <xs:element name="any3" minOccurs="0" type="xs:anyType"/>
          <xs:element name="any4" minOccurs="0" nillable="true" type="xs:anyType"/>
          <xs:element name="any5" maxOccurs="unbounded" type="xs:anyType"/>
          <xs:element name="any6" maxOccurs="unbounded" nillable="true" type="xs:anyType"/>      
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

  val builtInTypesXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SingularBuiltInTypeTest">
        <xs:sequence>
          <xs:element name="string" type="xs:string"/>
          <xs:element name="boolean" type="xs:boolean"/>
          <xs:element name="decimal" type="xs:decimal"/>
          <xs:element name="float" type="xs:float"/>
          <xs:element name="double" type="xs:double"/>
          <xs:element name="duration" type="xs:duration"/>
          <xs:element name="dateTime" type="xs:dateTime"/>
          <xs:element name="time" type="xs:time"/>
          <xs:element name="date" type="xs:date"/>
          <xs:element name="gYearMonth" type="xs:gYearMonth"/>

          <xs:element name="gYear" type="xs:gYear"/>
          <xs:element name="gMonthDay" type="xs:gMonthDay"/>
          <xs:element name="gDay" type="xs:gDay"/>
          <xs:element name="gMonth" type="xs:gMonth"/>
          <xs:element name="hexBinary" type="xs:hexBinary"/>
          <xs:element name="base64Binary" type="xs:base64Binary"/>
          <xs:element name="anyURI" type="xs:anyURI"/>
          <xs:element name="QName" type="xs:QName"/>
          <xs:element name="NOTATION" type="xs:NOTATION"/>
          <xs:element name="normalizedString" type="xs:normalizedString"/>

          <xs:element name="token" type="xs:token"/>
          <xs:element name="language" type="xs:language"/>
          <xs:element name="NMTOKEN" type="xs:NMTOKEN"/>
          <xs:element name="NMTOKENS" type="xs:NMTOKENS"/>
          <xs:element name="Name" type="xs:Name"/>
          <xs:element name="NCName" type="xs:NCName"/>
          <xs:element name="ID" type="xs:ID"/>
          <xs:element name="IDREF" type="xs:IDREF"/>
          <xs:element name="IDREFS" type="xs:IDREFS"/>
          <xs:element name="ENTITY" type="xs:ENTITY"/>

          <xs:element name="ENTITIES" type="xs:ENTITIES"/>
          <xs:element name="integer" type="xs:integer"/>
          <xs:element name="nonPositiveInteger" type="xs:nonPositiveInteger"/>
          <xs:element name="negativeInteger" type="xs:negativeInteger"/>
          <xs:element name="long" type="xs:long"/>
          <xs:element name="int" type="xs:int"/>
          <xs:element name="short" type="xs:short"/>
          <xs:element name="byte" type="xs:byte"/>
          <xs:element name="nonNegativeInteger" type="xs:nonNegativeInteger"/>
          <xs:element name="unsignedLong" type="xs:unsignedLong"/>

          <xs:element name="unsignedInt" type="xs:unsignedInt"/>
          <xs:element name="unsignedShort" type="xs:unsignedShort"/>
          <xs:element name="unsignedByte" type="xs:unsignedByte"/>
          <xs:element name="positiveInteger" type="xs:positiveInteger"/>
          <xs:element name="anyType" type="xs:anyType"/>
          <xs:element name="anySimpleType" type="xs:anySimpleType" />
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

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

      <xs:complexType name="EmptySequenceComplexTypeTest2">
        <xs:sequence>
          <xs:element name="int1" type="xs:int"/>
          <xs:sequence/>
        </xs:sequence>
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
          <xs:choice>
            <xs:element name="int1" type="xs:int"/>
            <xs:choice/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="EmptyChoiceComplexTypeTest">
        <xs:choice/>
      </xs:complexType>

      <xs:complexType name="EmptyChoiceComplexTypeTest2">
        <xs:sequence>
          <xs:element name="int1" type="xs:int"/>
          <xs:choice/>
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

  val namedGroupXML =
    <xs:schema targetNamespace="http://www.example.com/ipo"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:ipo="http://www.example.com/ipo">
      <xs:group name="emptySeq">
        <xs:sequence/>
      </xs:group>
      <xs:group name="seq">
        <xs:sequence>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:group>      
    </xs:schema>

  val grouprefXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="EmptySequenceGroupTest">
        <xs:sequence>
          <xs:group ref="gen:emptySeq"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="EmptyGroupChoiceTest">
        <xs:choice>
          <xs:group ref="gen:emptySeq"/>
          <xs:element name="string1" type="xs:string"/>
        </xs:choice>
      </xs:complexType>      

      <xs:complexType name="Array" >
        <xs:annotation>
        <xs:documentation>
         'Array' is a complex type for accessors identified by position 
        </xs:documentation>
      </xs:annotation>
        <xs:group ref="gen:Array" minOccurs="0" />
      </xs:complexType>

      <xs:group name="Array" >
        <xs:sequence>
          <xs:any namespace="##any" minOccurs="0" maxOccurs="unbounded" processContents="lax" />
        </xs:sequence>
      </xs:group>

      <xs:group name="emptySeq">
        <xs:sequence/>
      </xs:group>

      <xs:group name="seq">
        <xs:sequence>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:group>       
    </xs:schema>
  
  val mixedXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="TopLevelMultipleSeqAnyTest" mixed="true">
        <xs:sequence minOccurs="0" maxOccurs="unbounded">
          <xs:any namespace="##other" processContents="lax"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>

  val attributeXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>

      <xs:element name="attributeTest">
        <xs:complexType>
          <xs:attribute name="milk1" type="gen:MilkType"/>
          <xs:attribute name="string2" type="xs:string"/>
          <xs:anyAttribute namespace="##any"/>
        </xs:complexType>
      </xs:element>

      <xs:complexType name="anySimpleTypeExtension">
        <xs:simpleContent>
          <xs:extension base="xs:anySimpleType">
             <xs:anyAttribute namespace="##any" processContents="lax"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>
    </xs:schema>

  val derivationXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="numberlist1" type="gen:ListOfUnsignedInt"/>
          <xs:element name="milklist1" type="gen:ListOfMilk"/>
        </xs:sequence>
      </xs:complexType>      

      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>

      <xs:simpleType name="ListOfMilk">
        <xs:list itemType="gen:MilkType"/>
      </xs:simpleType>

      <xs:simpleType name="ListOfMilkDerived">
        <xs:restriction base="gen:ListOfMilk">
          <xs:minLength value="0"/>
          <xs:maxLength value="1"/>
        </xs:restriction>
      </xs:simpleType>

      <xs:complexType name="ComplexListOfMilk">
        <xs:simpleContent>
          <xs:extension base="gen:ListOfMilkDerived">
            <xs:attributeGroup ref="gen:coreattrs"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>

      <xs:complexType name="ComplexMilkDerived">
        <xs:simpleContent>
          <xs:restriction base="gen:ComplexMilk">
          </xs:restriction>
        </xs:simpleContent>
      </xs:complexType>

      <xs:complexType name="ComplexMilk">
        <xs:simpleContent>
          <xs:extension base="gen:MilkType">
            <xs:attributeGroup ref="gen:coreattrs"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>

      <xs:simpleType name="ListOfUnsignedInt">
        <xs:list itemType="xs:unsignedInt"/>
      </xs:simpleType>

      <xs:simpleType name="ListOfUnsignedIntDerived">
        <xs:restriction base="gen:ListOfUnsignedInt">
          <xs:minLength value="0"/>
          <xs:maxLength value="1"/>
        </xs:restriction>
      </xs:simpleType>

      <xs:complexType name="ComplexListOfBuiltInType">
        <xs:simpleContent>
          <xs:extension base="gen:ListOfUnsignedIntDerived">
            <xs:attributeGroup ref="gen:coreattrs"/>
          </xs:extension>
        </xs:simpleContent>
      </xs:complexType>

      <xs:attributeGroup name="coreattrs">
        <xs:annotation>
          <xs:documentation>
          core attributes common to most elements
          id       document-wide unique id
          </xs:documentation>
        </xs:annotation>
        <xs:attribute name="id" type="xs:ID"/>
        <xs:attribute name="class" type="xs:NMTOKENS"/>
      </xs:attributeGroup>      
    </xs:schema>

  val addressXML =
    <xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
          <xs:element name="state" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="USAddress">
        <xs:complexContent>
          <xs:extension base="gen:Address">
            <xs:sequence>
              <xs:element name="zip"   type="xs:positiveInteger"/>
            </xs:sequence>
            <xs:attribute name="href" type="xs:anyURI"/>
            <xs:anyAttribute/>
          </xs:extension>
        </xs:complexContent>
      </xs:complexType>

      <xs:complexType name="Array" >
        <xs:annotation>
        <xs:documentation>
         'Array' is a complex type for accessors identified by position 
        </xs:documentation>
      </xs:annotation>
        <xs:group ref="gen:Array" minOccurs="0" />
      </xs:complexType>

      <xs:group name="Array" >
        <xs:sequence>
          <xs:any namespace="##any" minOccurs="0" maxOccurs="unbounded" processContents="lax" />
        </xs:sequence>
      </xs:group>
    </xs:schema>

  val importXML =
    <xs:schema targetNamespace="http://www.example.com/general_import"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general"
        xmlns:im="http://www.example.com/general_import"
        elementFormDefault="qualified">
      <xs:import namespace="http://www.example.com/general"
            schemaLocation="http://www.example.com/general"/>
      <xs:element name="SubGroupMember3" type="xs:string" substitutionGroup="gen:subgroupHead"/>

      <xs:complexType name="IntlAddress">
        <xs:complexContent>
          <xs:extension base="gen:Address">
            <xs:sequence>
              <xs:element name="postalCode"   type="xs:positiveInteger"/>
              <xs:element name="country"   type="xs:string"/>
            </xs:sequence>
          </xs:extension>
        </xs:complexContent>
      </xs:complexType>

      <xs:complexType name="RestrictedArray">
        <xs:complexContent>
          <xs:restriction base="gen:Array">
          </xs:restriction>
        </xs:complexContent>
      </xs:complexType>  
    </xs:schema>
}
