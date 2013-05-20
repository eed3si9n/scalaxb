package scalaxb.specs

import org.specs2._

object EntitySpec_1 extends Specification { def is = sequential               ^
  "sequences in a complex type should"                                        ^
    "generate a case class named FooSequence* for non-primary sequences"      ! seq1^
    "be referenced as fooSequence within the type"                            ! seq2^
    "not generate anything when the primary sequence is empty"                ! seq3^
    "generate a case class if the primary sequence is either optional or multiple" ! seq4^
    "be split into chunks of case classes when it exceeds 20 particles"       ! seq5^
    "generate accessors for elements within the wrapped sequence"             ! seq6^
                                                                              end^
  "choices in a complex type should"                                          ^
    "generate a trait named FooOption*"                                       ! choice1^
    "mixin FooOption to choice candidates"                                    ! choice3^
    "be referenced as DataRecord[FooOption] if it's made of non-nillable complex type element" ! choice2^
    "be referenced as DataRecord[Option[FooOption]] if it's made of complex types, some nillable" ! choice2^
    "be referenced as DataRecord[Int] if it's made of xs:int"                 ! choice2^
                                                                              end^
  "an all in a complex type should"                                           ^
    "be referenced as Map[String, scalaxb.DataRecord[Any]]"                   ! all1^
                                                                              end^
  "a groupref in a complex type should"                                       ^
    "be referenced as the group's primary compositor"                         ! groupref1^
                                                                              end^
  "wildcards should"                                                          ^
    "be referenced as DataRecord[Any] named any*"                             ! wildcard1^
    "be referenced as Option[DataRecord[Any]] if optional"                    ! wildcard2^
    "be referenced as Seq[DataRecord[Any]] if maxOccurs >1"                   ! wildcard2^
                                                                              end^
  "a single particle with maxOccurs >1 should"                                ^
    "be referenced as A*"                                                     ! param1^
                                                                              end^
  "substitution groups should"                                                ^
    "be referenced as the group head's type"                                  ! sub1^
                                                                              end^
  "attriubtes should"                                                         ^
    "be referenced as Map[String, scalaxb.DataRecord[Any]]"                   ! attr1^
    "generate an accessor"                                                    ! attr2^
                                                                              end^
  "attribute groups should"                                                   ^
    "generate a trait with attribute accessor signatures"                     ! attributegroup1^
    "generate accessors in the referencing complex type"                      ! attributegroup2^
    "be extended by the referencing complex types"                            ! attributegroup3^
                                                                              end

  import Example._
  // scalaxb.compiler.Module.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver
  
  lazy val seqEntitySource = module.processNode(sequenceXML, "example")(0)

  val seqExpectedSequenceTest =
    """case class SequenceComplexTypeTest(sequencecomplextypetestsequence: example.SequenceComplexTypeTestSequence, """ +
      """sequencecomplextypetestsequence2: example.SequenceComplexTypeTestSequence2, """ +
      """sequencecomplextypetestsequence3: Option[example.SequenceComplexTypeTestSequence3], """ +
      """sequencecomplextypetestsequence4: Option[example.SequenceComplexTypeTestSequence4], """ +
      """sequencecomplextypetestsequence5: Seq[example.SequenceComplexTypeTestSequence5], """ +
      """sequencecomplextypetestsequence6: Seq[example.SequenceComplexTypeTestSequence6], """ +
      """sequencecomplextypetestsequence7: example.SequenceComplexTypeTestSequence7)"""

  def seq1 = {
    println(seqEntitySource)
    seqEntitySource must contain("""case class SequenceComplexTypeTestSequence""")
  }

  def seq2 = {
    seqEntitySource must contain(seqExpectedSequenceTest)
  }

  def seq3 = {
    seqEntitySource must contain("""case class EmptySequenceComplexTypeTest""")
  }

  def seq4 = {
    seqEntitySource must contain("""case class MultipleSequenceComplexTypeTest(""" +
      """multiplesequencecomplextypetestsequence: example.MultipleSequenceComplexTypeTestSequence*)""")
  }

  val seqExpectedLongSequenceTest = 
    """case class LongSequenceComplexTypeTest(longsequencecomplextypetestsequence: example.LongSequenceComplexTypeTestSequence, """

  def seq5 =
    (seqEntitySource must contain(seqExpectedLongSequenceTest)) and
    (seqEntitySource must contain("""case class LongSequenceComplexTypeTestSequence(int1: Int"""))

  def seq6 = {
    seqEntitySource must contain("""case class LongSequenceComplexTypeTestSequence(int1: Int""")
  }

  lazy val choiceEntitySource = module.processNode(choiceXML, "example")(0)

  val expectedChoiceTest =
    """case class ChoiceComplexTypeTest(choicecomplextypetestoption: scalaxb.DataRecord[example.ChoiceComplexTypeTestOption], """ +
      """choicecomplextypetestoption2: scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption2]], """ +
      """choicecomplextypetestoption3: Option[scalaxb.DataRecord[example.ChoiceComplexTypeTestOption3]], """ +
      """choicecomplextypetestoption4: Option[scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption4]]], """ +
      """choicecomplextypetestoption5: Seq[scalaxb.DataRecord[example.ChoiceComplexTypeTestOption5]], """ +
      """choicecomplextypetestoption6: Seq[scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption6]]], """ +
      """choicecomplextypetestoption7: scalaxb.DataRecord[Int])"""

  def choice1 = {
    println(choiceEntitySource)
    choiceEntitySource must contain("""trait ChoiceComplexTypeTestOption""")
  }

  def choice2 = {
    choiceEntitySource must contain(expectedChoiceTest)
  }

  def choice3 = {
    choiceEntitySource must contain("case class Address(street: String, city: String) extends example.ChoiceComplexTypeTestOption " +
      "with example.ChoiceComplexTypeTestOption2 " +
      "with example.ChoiceComplexTypeTestOption3 " +
      "with example.ChoiceComplexTypeTestOption4 " +
      "with example.ChoiceComplexTypeTestOption5 " +
      "with example.ChoiceComplexTypeTestOption6")
  }

  def all1 = {
    val entitySource = module.processNode(allXML, "example")(0)

    println(entitySource)
    entitySource must contain("""case class AllComplexTypeTest(all: Map[String, scalaxb.DataRecord[Any]])""")
  }

  def groupref1 = {
    val entitySource = module.processNode(grouprefXML, "example")(0)

    println(entitySource)
    (entitySource must contain("""case class EmptySequenceGroupTest(emptyseqgroupsequence: example.EmptySeqGroupSequence)""")) and
    (entitySource must contain("""case class ArrayType(arraysequence: Option[example.ArraySequence])"""))
  }

  lazy val wildcardEntitySource = module.processNode(wildcardXML, "example")(0)

  val exptectedAnyTest =
    """case class WildcardTest(person1: example.Person, """ +
      """any: scalaxb.DataRecord[Any], """ +
      """any2: scalaxb.DataRecord[Any], """ +
      """any3: scalaxb.DataRecord[Any], """ +
      """any4: scalaxb.DataRecord[Any], """ +
      """wildcardtestoption: scalaxb.DataRecord[scalaxb.DataRecord[Any]], """ +
      """person3: Option[example.Person], """ +
      """any5: Option[scalaxb.DataRecord[Any]], """ +
      """wildcardtestoption2: Option[scalaxb.DataRecord[scalaxb.DataRecord[Any]]], """ +
      """any6: Seq[scalaxb.DataRecord[Any]], """ +
      """person5: Seq[example.Person], """ +
      """any7: scalaxb.DataRecord[Any])"""
  
  def wildcard1 = {
    println(wildcardEntitySource)
    wildcardEntitySource must contain(exptectedAnyTest)
  }

  def wildcard2 = {
    wildcardEntitySource must contain(exptectedAnyTest)
  }

  def param1 = {
    val entitySource = module.processNode(seqParamXML, "example")(0)

    println(entitySource)
    entitySource.lines.toList must contain(
      """case class SeqParamTest(foo: String*)""",
      """case class NillableSeqParamTest(foo: Option[String]*)"""
    )
  }

  def sub1 = {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>

      <xs:element name="SubstitutionGroup" type="xs:anyType" abstract="true"/>
      <xs:element name="SubGroupMember" type="gen:MilkType" substitutionGroup="gen:SubstitutionGroup"/>
      <xs:element name="SubGroupMember2" type="xs:int" substitutionGroup="gen:SubstitutionGroup"/>

      <xs:complexType name="SubstitutionGroupTest">
        <xs:sequence>
          <xs:element ref="gen:SubstitutionGroup"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    println(entitySource)
    entitySource must contain("""case class SubstitutionGroupTest(SubstitutionGroup: scalaxb.DataRecord[Any])""")
  }

  lazy val attrEntitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
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
    </xs:schema>, "example")(0)

  def attr1 = {
    println(attrEntitySource)
    attrEntitySource must contain("""case class AttributeTest(attributes: Map[String, scalaxb.DataRecord[Any]])""")
  }

  def attr2 = {
    attrEntitySource must contain("""lazy val milk1: Option[example.MilkType] = attributes.get("@milk1").map(_.as[example.MilkType])""")
  }

  lazy val attributeGroupEntitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:attributeGroup name="coreattrs">
        <xs:attribute name="id" type="xs:ID"/>
        <xs:attribute name="class" type="xs:NMTOKENS"/>
      </xs:attributeGroup>

      <xs:element name="attributeGroupTest">
        <xs:complexType>
          <xs:attributeGroup ref="gen:coreattrs"/>
        </xs:complexType>
      </xs:element>
    </xs:schema>, "example")(0)

  def attributegroup1 = {
    println(attributeGroupEntitySource)
    attributeGroupEntitySource.lines.toList must contain(
      """trait Coreattrs {""",
      """  def id: Option[String]""").inOrder
  }

  def attributegroup2 = {
    attributeGroupEntitySource must contain("""lazy val id: Option[String] = attributes.get("@id").map(_.as[String])""")
  }

  def attributegroup3 = {
    attributeGroupEntitySource must contain("""case class AttributeGroupTest(attributes: Map[String, scalaxb.DataRecord[Any]]) extends example.Coreattrs""")
  }
}
