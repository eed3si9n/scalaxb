package scalaxb.specs

import org.specs2._

object EntitySpec_1 extends Specification { def is =                          s2"""
  sequences in a complex type should
    generate a case class named FooSequence* for non-primary sequences        $seq1
    be referenced as fooSequence within the type                              $seq2
    not generate anything it is empty, and skipped from the parameters        $seq3
    generate a case class if the primary sequence is either optional or multiple $seq4
    generate a case class for sequences with length > 22                      $seq5
  
  choices in a complex type should
    generate a trait named FooOption*                                         $choice1
    mixin FooOption to choice candidates                                      $choice3
    not generate anything if it is empty, and skipped from the parameters     $choice4
    be referenced as DataRecord[X] if it's made of non-nillable complex type elements $choice2
    be referenced as DataRecord[Option[X]] if it includes nillable elements   $choice2
    be referenced as DataRecord[Option[X]] if it includes empty compositor    $choice2
    be referenced as DataRecord[Int] if it's made of xs:int                   $choice2
  
  an all in a complex type should
    be referenced as Map[String, scalaxb.DataRecord[Any]]                     $all1
  
  a groupref in a complex type should
    be referenced as the group's non-empty primary compositor named FooGroup   $groupref1
    be referenced as Option[scalaxb.DataRecord[X]] if underlying choice includes empty compositor $groupref2
    be referenced as scalaxb.DataRecord[Option[X]] if underlying choice includes nillable element $groupref3
  
  wildcards should
    be referenced as DataRecord[Any] named any* if it's made of non-nillable elements $wildcard1
    be referenced as DataRecord[Option[Any]] it includes nillable elements    $wildcard2
    be referenced as Option[DataRecord[Any]] if optional                      $wildcard2
    be referenced as Seq[DataRecord[Any]] if maxOccurs >1                     $wildcard2
  
  a single particle with maxOccurs >1 should
    be referenced as A*                                                       $param1
  
  mixed contents in a complex type should
    generate a parameter named mixed in place of normal parameters            $mixed1
    except when it hold simple contents                                       $mixed2
  
  substitution groups should
    be referenced as the group head's type                                    $sub1
  
  attriubtes should
    be referenced as Map[String, scalaxb.DataRecord[Any]]                     $attr1
    generate an accessor                                                      $attr2
  
  attribute groups should
    generate a trait with attribute accessor signatures                       $attributegroup1
    generate accessors in the referencing complex type                        $attributegroup2
    be extended by the referencing complex types                              $attributegroup3"""

  import Example._
  //scalaxb.compiler.Log.configureLogger(true)
  lazy val module = new scalaxb.compiler.xsd2.Driver
  
  lazy val seqEntitySource = module.processNode(sequenceXML, "example")(0)

  val seqExpectedSequenceTest =
    """case class SequenceComplexTypeTest(sequenceComplexTypeTestSequence: example.SequenceComplexTypeTestSequence, """ +
      """sequenceComplexTypeTestSequence2: example.SequenceComplexTypeTestSequence2, """ +
      """sequenceComplexTypeTestSequence3: Option[example.SequenceComplexTypeTestSequence3], """ +
      """sequenceComplexTypeTestSequence4: Option[example.SequenceComplexTypeTestSequence4], """ +
      """sequenceComplexTypeTestSequence5: Seq[example.SequenceComplexTypeTestSequence5], """ +
      """sequenceComplexTypeTestSequence6: Seq[example.SequenceComplexTypeTestSequence6], """ +
      """sequenceComplexTypeTestSequence7: example.SequenceComplexTypeTestSequence7)"""

  def seq1 = {
    println(seqEntitySource)
    seqEntitySource must contain("""case class SequenceComplexTypeTestSequence""")
  }

  def seq2 = {
    seqEntitySource must contain(seqExpectedSequenceTest)
  }

  def seq3 = {
    (seqEntitySource must contain("""case class EmptySequenceComplexTypeTest""")) and
    (seqEntitySource must contain("""case class EmptySequenceComplexTypeTest2(int1: Int)"""))
  }

  def seq4 = {
    seqEntitySource must contain("""case class MultipleSequenceComplexTypeTest(""" +
      """multipleSequenceComplexTypeTestSequence: example.MultipleSequenceComplexTypeTestSequence*)""")
  }

  def seq5 = {
    seqEntitySource must contain("""case class LongSequenceComplexTypeTest(int1: Int""")
  }

  lazy val choiceEntitySource = module.processNode(choiceXML, "example")(0)

  val expectedChoiceTest =
    """case class ChoiceComplexTypeTest(choiceComplexTypeTestOption: scalaxb.DataRecord[example.ChoiceComplexTypeTestOption], """ +
      """choiceComplexTypeTestOption2: scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption2]], """ +
      """choiceComplexTypeTestOption3: Option[scalaxb.DataRecord[example.ChoiceComplexTypeTestOption3]], """ +
      """choiceComplexTypeTestOption4: Option[scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption4]]], """ +
      """choiceComplexTypeTestOption5: Seq[scalaxb.DataRecord[example.ChoiceComplexTypeTestOption5]], """ +
      """choiceComplexTypeTestOption6: Seq[scalaxb.DataRecord[Option[example.ChoiceComplexTypeTestOption6]]], """ +
      """choiceComplexTypeTestOption7: scalaxb.DataRecord[Int], """ +
      """choiceComplexTypeTestOption8: Option[scalaxb.DataRecord[Any]])"""

  def choice1 = {
    println(choiceEntitySource)
    choiceEntitySource must contain("""trait ChoiceComplexTypeTestOption""")
  }

  def choice2 = {
    choiceEntitySource must contain(expectedChoiceTest)
  }

  def choice3 = {
    (choiceEntitySource must contain("case class Address(street: String, city: String) extends example.ChoiceComplexTypeTestOption " +
      "with example.ChoiceComplexTypeTestOption2 " +
      "with example.ChoiceComplexTypeTestOption3 " +
      "with example.ChoiceComplexTypeTestOption4 " +
      "with example.ChoiceComplexTypeTestOption5 " +
      "with example.ChoiceComplexTypeTestOption6")) and
    (choiceEntitySource must contain("""case class ChoiceComplexTypeTestSequence(string1: String, string2: String) extends example.ChoiceComplexTypeTestOption"""))
  }

  def choice4 = {
    (choiceEntitySource must not contain("""trait EmptyChoiceComplexTypeTestOption""")) and
    (choiceEntitySource must not contain("""trait EmptyChoiceComplexTypeTest2Option"""))
  }

  def all1 = {
    val entitySource = module.processNode(allXML, "example")(0)

    println(entitySource)
    entitySource must contain("""case class AllComplexTypeTest(all: Map[String, scalaxb.DataRecord[Any]])""")
  }


  lazy val grouprefEntitySource = module.processNode(grouprefXML, "example")(0)

  def groupref1 = {    
    println(grouprefEntitySource)
    (grouprefEntitySource must not contain """case class EmptySeqGroup""") and
    (grouprefEntitySource must contain("""case class EmptySequenceGroupTest""")) and
    (grouprefEntitySource must contain("""case class ArrayType(arrayGroup: Option[example.ArrayGroup])"""))
  }

  def groupref2 = {
    (grouprefEntitySource must contain("""case class EmptyGroupChoiceTest(emptyGroupChoiceTestOption: Option[scalaxb.DataRecord[Any]])"""))
  }

  def groupref3 = {
    (grouprefEntitySource must contain("""case class NillableChoiceGroupTest(nillableChoiceGroupGroup: scalaxb.DataRecord[Option[example.NillableChoiceGroupGroup]])"""))
  }

  lazy val wildcardEntitySource = module.processNode(wildcardXML, "example")(0)

  val exptectedWildcardTest =
    """case class WildcardTest(person1: example.Person, """ +
      """any: scalaxb.DataRecord[Any], """ +
      """any2: scalaxb.DataRecord[Any], """ +
      """any3: scalaxb.DataRecord[Any], """ +
      """any4: scalaxb.DataRecord[Any], """ +
      """wildcardTestOption: scalaxb.DataRecord[Option[Any]], """ +
      """person3: Option[example.Person], """ +
      """any5: Option[scalaxb.DataRecord[Any]], """ +
      """wildcardTestOption2: Option[scalaxb.DataRecord[Option[Any]]], """ +
      """any6: Seq[scalaxb.DataRecord[Any]], """ +
      """person5: Seq[example.Person], """ +
      """any7: scalaxb.DataRecord[Any])"""
  
  def wildcard1 = {
    println(wildcardEntitySource)
    wildcardEntitySource must contain(exptectedWildcardTest)
  }

  def wildcard2 = {
    wildcardEntitySource must contain(exptectedWildcardTest)
  }

  def param1 = {
    val entitySource = module.processNode(seqParamXML, "example")(0)

    println(entitySource)
    entitySource.lines.toList must contain(allOf(
      """case class SeqParamTest(foo: String*)""",
      """case class NillableSeqParamTest(foo: Option[String]*)"""
    ))
  }

  lazy val mixedEntitySource = module.processNode(mixedXML, "example")(0)

  def mixed1 = {
    println(mixedEntitySource)
    mixedEntitySource must contain ("case class TopLevelMultipleSeqAnyTest(mixed: Seq[scalaxb.DataRecord[Any]])")
  }

  def mixed2 = {
    mixedEntitySource must contain ("case class MixedStringExtensionTest(value: String)")
  }

  def sub1 = {
    val entitySource = module.processNode(subXML, "example")(0)

    println(entitySource)
    entitySource must contain("""case class SubstitutionGroupTest(SubstitutionGroup: scalaxb.DataRecord[Any])""")
  }

  lazy val attrEntitySource = module.processNode(attributeXML, "example")(0)

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
    attributeGroupEntitySource.lines.toList must contain(allOf(
      """trait Coreattrs {""",
      """  def id: Option[String]""")).inOrder
  }

  def attributegroup2 = {
    attributeGroupEntitySource must contain("""lazy val id: Option[String] = attributes.get("@id").map(_.as[String])""")
  }

  def attributegroup3 = {
    attributeGroupEntitySource must contain("""case class AttributeGroupTest(attributes: Map[String, scalaxb.DataRecord[Any]]) extends example.Coreattrs""")
  }
}
