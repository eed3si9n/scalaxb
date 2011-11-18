/*
 * Copyright (c) 2010 e.e d3si9n
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler.xsd2

import java.net.{URI}
import scala.xml.{NamespaceBinding}
import scalaxb._
import scalaxb.compiler.xsd.{XsTypeSymbol, XsAnyType}
import xmlschema._
import scala.collection.mutable.{Builder, ArrayBuffer}
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec

object Defs {
  implicit def schemaToSchemaIteration(schema: XSchema): SchemaIteration = SchemaIteration(schema)
  implicit def complexTypeToComplexTypeOps(tagged: Tagged[XComplexType]): ComplexTypeOps = ComplexTypeOps(tagged)
  implicit def complexTypeToComplexTypeIteration(tagged: Tagged[XComplexType])(implicit schema: XSchema): ComplexTypeIteration =
    ComplexTypeIteration(tagged)
  implicit def elementToElementOps(tagged: Tagged[XElement]): ElementOps = new ElementOps(tagged)
  implicit def attributeGroupToAttributeGroupOps(tagged: Tagged[XAttributeGroup]): AttributeGroupOps =
    new AttributeGroupOps(tagged)
  
  val XML_SCHEMA_URI = new URI("http://www.w3.org/2001/XMLSchema")
  val XSI_URL = new URI("http://www.w3.org/2001/XMLSchema-instance")
  val XSI_PREFIX = "xsi"
  val XML_URI = new URI("http://www.w3.org/XML/1998/namespace")
  val XML_PREFIX = "xml"
  val SCALA_URI = new URI("http://scala-lang.org/")
  val SCALAXB_URI = new URI("http://scalaxb.org/")
  val NL = System.getProperty("line.separator")

  val XS_ANY_TYPE = QualifiedName(XML_SCHEMA_URI, "anyType")
  val XS_ANY_SIMPLE_TYPE = QualifiedName(XML_SCHEMA_URI, "anySimpleType")

  val GroupTag = "group"
  val ChoiceTag = "choice"
  val SequenceTag = "sequence"
  val AllTag = "all"
}

abstract class TopLevelType
case object SimpleTypeHost extends  TopLevelType
case object ComplexTypeHost extends TopLevelType
case object NamedGroupHost extends TopLevelType
case object AttributeGroupHost extends TopLevelType
case object ElementHost extends TopLevelType
case object AttributeHost extends TopLevelType
case class HostTag(namespace: Option[URI], topLevel: TopLevelType, name: String)
object HostTag {
  def apply(namespace: Option[URI], elem: XTopLevelElement): HostTag =
    HostTag(namespace, ElementHost, elem.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelSimpleType): HostTag =
    HostTag(namespace, SimpleTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelComplexType): HostTag =
    HostTag(namespace, ComplexTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], attr: XTopLevelAttribute): HostTag =
    HostTag(namespace, AttributeHost, attr.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedGroup): HostTag =
    HostTag(namespace, NamedGroupHost, group.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedAttributeGroup): HostTag =
    HostTag(namespace, AttributeGroupHost, group.name getOrElse {error("name is required.")})
}

case class KeyedGroup(key: String, group: XGroup) {
  import Defs._

  // List of TaggedElement, TaggedKeyedGroup, or TaggedAny.
  def particles(implicit tag: HostTag, lookup: Lookup, splitter: Splitter): Seq[Tagged[_]] =
    group.arg1.toSeq flatMap {
      case DataRecord(_, _, x: XLocalElementable) =>
        Seq(TaggedLocalElement(x, lookup.schema.unbound.elementFormDefault, tag))
      case DataRecord(_, Some(particleKey), x: XGroupRef) => Seq(Tagged(KeyedGroup(particleKey, x), tag))
      case DataRecord(_, Some(particleKey), x: XExplicitGroupable) =>
        if (particleKey == SequenceTag) KeyedGroup(particleKey, x).innerSequenceToParticles
        else Seq(Tagged(KeyedGroup(particleKey, x), tag))
      case DataRecord(_, _, x: XAny) => Seq(Tagged(x, tag))
    }

  private def innerSequenceToParticles(implicit tag: HostTag, lookup: Lookup, splitter: Splitter): Seq[Tagged[_]] =
    if (group.minOccurs != 1 || group.maxOccurs != "1")
      if (group.arg1.length == 1) group.arg1(0) match {
        case DataRecord(_, Some(particleKey), any: XAny) =>
          Seq(Tagged(any.copy(
            minOccurs = math.min(any.minOccurs.toInt, group.minOccurs.toInt),
            maxOccurs = Occurrence.max(any.maxOccurs, group.maxOccurs)), tag))
        case DataRecord(_, Some(ChoiceTag), choice: XExplicitGroup) =>
          Seq(Tagged(KeyedGroup(ChoiceTag, choice.copy(
            minOccurs = math.min(choice.minOccurs.toInt, group.minOccurs.toInt),
            maxOccurs = Occurrence.max(choice.maxOccurs, group.maxOccurs)) ), tag))

        case _ => Seq(Tagged(this, tag))
      }
      else Seq(Tagged(this, tag))
    else splitter.splitIfLongSequence(Tagged(this, tag))
}

/** represents attributes param */
case class AttributeSeqParam() {}

/** represents param created for xs:all. */
case class AllParam() {}

sealed trait Tagged[+A] {
  def value: A
  def tag: HostTag
  override def toString: String = "Tagged(%s, %s)".format(value.toString, tag.toString)
}

sealed trait TaggedAttr[+A] extends Tagged[A] {}

object Tagged {
  def apply(value: XSimpleType, tag: HostTag): Tagged[XSimpleType] = TaggedSimpleType(value, tag)
  def apply(value: XComplexType, tag: HostTag): Tagged[XComplexType] = TaggedComplexType(value, tag)
  def apply(value: KeyedGroup, tag: HostTag): Tagged[KeyedGroup] = TaggedKeyedGroup(value, tag)
  def apply(value: XAttributeGroup, tag: HostTag): TaggedAttr[XAttributeGroup] = TaggedAttributeGroup(value, tag)
  def apply(value: XTopLevelElement, tag: HostTag): Tagged[XTopLevelElement] = TaggedTopLevelElement(value, tag)
  def apply(value: XLocalElementable, elementFormDefault: XFormChoice, tag: HostTag): Tagged[XLocalElementable] =
    TaggedLocalElement(value, elementFormDefault, tag)
  def apply(value: XAttributable, tag: HostTag): TaggedAttr[XAttributable] = TaggedAttribute(value, tag)
  def apply(value: XAny, tag: HostTag): Tagged[XAny] = TaggedWildCard(value, tag)
  def apply(value: XsTypeSymbol, tag: HostTag): Tagged[XsTypeSymbol] = TaggedSymbol(value, tag)
  def apply(value: XNoFixedFacet, tag: HostTag): Tagged[XNoFixedFacet] = TaggedEnum(value, tag)
  def apply(value: AttributeSeqParam, tag: HostTag): Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(value, tag)
  def apply(value: AllParam, tag: HostTag): Tagged[AllParam] = TaggedAllParam(value, tag)

  implicit def box(value: XSimpleType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XComplexType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: KeyedGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttributeGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XTopLevelElement)(implicit tag: HostTag) = Tagged(value, tag)
  // implicit def box(value: XLocalElementable)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttributable)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAny)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XsTypeSymbol)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XNoFixedFacet)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: AttributeSeqParam)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: AllParam)(implicit tag: HostTag) = Tagged(value, tag)

  implicit def unbox[A](tagged: Tagged[A]): A = tagged.value

  def toParticleDataRecord(tagged: Tagged[_]): DataRecord[XParticleOption] = tagged match {
    case TaggedLocalElement(value, _, tag) => DataRecord(tag.namespace map {_.toString}, Some(tag.name), value)
    case _ => error("unknown particle: " + tagged)
  }
}

case class TaggedSimpleType(value: XSimpleType, tag: HostTag) extends Tagged[XSimpleType] {}
case class TaggedComplexType(value: XComplexType, tag: HostTag) extends Tagged[XComplexType] {}
case class TaggedKeyedGroup(value: KeyedGroup, tag: HostTag) extends Tagged[KeyedGroup] {}
case class TaggedAttributeGroup(value: XAttributeGroup, tag: HostTag) extends TaggedAttr[XAttributeGroup] {}
case class TaggedTopLevelElement(value: XTopLevelElement, tag: HostTag) extends Tagged[XTopLevelElement] {}
case class TaggedLocalElement(value: XLocalElementable, elementFormDefault: XFormChoice,
                              tag: HostTag) extends Tagged[XLocalElementable] {}
case class TaggedAttribute(value: XAttributable, tag: HostTag) extends TaggedAttr[XAttributable] {}
case class TaggedAnyAttribute(value: XWildcardable, tag: HostTag) extends TaggedAttr[XWildcardable] {}
case class TaggedWildCard(value: XAny, tag: HostTag) extends Tagged[XAny] {}
case class TaggedSymbol(value: XsTypeSymbol, tag: HostTag) extends Tagged[XsTypeSymbol] {}
case class TaggedEnum(value: XNoFixedFacet, tag: HostTag) extends Tagged[XNoFixedFacet] {}
case class TaggedDataRecordSymbol(value: DataRecordSymbol) extends Tagged[DataRecordSymbol] {
  import Defs._
  val tag = HostTag(Some(SCALAXB_URI), SimpleTypeHost, "DataRecord")
}
object TaggedXsAnyType extends TaggedSymbol(XsAnyType, HostTag(Some(Defs.SCALAXB_URI), SimpleTypeHost, "anyType"))
case class TaggedAttributeSeqParam(value: AttributeSeqParam, tag: HostTag) extends Tagged[AttributeSeqParam] {}
case class TaggedAllParam(value: AllParam, tag: HostTag) extends Tagged[AllParam] {}

case class DataRecordSymbol(member: Tagged[Any]) extends XsTypeSymbol {
  val name = "DataRecordSymbol(" + member + ")"
}

class SchemaIteration(underlying: Seq[Tagged[_]]) extends scala.collection.IndexedSeqLike[Tagged[_], SchemaIteration] {
  lazy val length: Int = underlying.length
  def apply(index: Int): Tagged[_] = underlying(index)

  override def isEmpty = underlying.isEmpty
  override def toSeq = underlying.toSeq
  override def seq = underlying.seq
  override def iterator = underlying.iterator
  override protected[this] def newBuilder: Builder[Tagged[_], SchemaIteration] = SchemaIteration.newBuilder
}

object SchemaIteration {
  import Defs._

  def apply(schema: XSchema): SchemaIteration = new SchemaIteration(schemaToSeq(schema))
  def fromSeq(seq: Seq[Tagged[_]]): SchemaIteration = new SchemaIteration(seq)

  def newBuilder: Builder[Tagged[_], SchemaIteration] = new ArrayBuffer[Tagged[_]] mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[SchemaIteration, Tagged[_], SchemaIteration] =
    new CanBuildFrom[SchemaIteration, Tagged[_], SchemaIteration] {
      def apply(): Builder[Tagged[_], SchemaIteration] = newBuilder
      def apply(from: SchemaIteration): Builder[Tagged[_], SchemaIteration] = newBuilder
    }

  def toThat(decl: XSimpleType, tag: HostTag): Option[Tagged[_]] = Some(Tagged(decl, tag))
  def toThat(decl: XComplexType, tag: HostTag): Option[Tagged[_]] = Some(Tagged(decl, tag))
  def toThat(group: KeyedGroup, tag: HostTag): Option[Tagged[_]] = Some(Tagged(group, tag))
  def toThat(group: XAttributeGroup, tag: HostTag): Option[Tagged[_]] = Some(Tagged(group, tag))
  def toThat(elem: XTopLevelElement, tag: HostTag): Option[Tagged[_]] = Some(Tagged(elem, tag))
  def toThat(elem: XLocalElementable, elementFormDefault: XFormChoice, tag: HostTag): Option[Tagged[_]] =
    Some(Tagged(elem, elementFormDefault, tag))
  def toThat(attr: XAttributable, tag: HostTag): Option[Tagged[_]] = Some(Tagged(attr, tag))

  def schemaToSeq(schema: XSchema): Seq[Tagged[_]] = {
    implicit val s = schema
    val ns = schema.targetNamespace

    // <xs:element ref="xs:simpleType"/>
    // <xs:element ref="xs:complexType"/>
    // <xs:element ref="xs:group"/>
    // <xs:element ref="xs:attributeGroup"/>
    // <xs:element ref="xs:element"/>
    // <xs:element ref="xs:attribute"/>
    // <xs:element ref="xs:notation"/>
    schema.xschemasequence1.toSeq flatMap {
      case XSchemaSequence1(data, _) => data match {
        case DataRecord(_, _, x: XTopLevelSimpleType)  =>
          processSimpleType(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelComplexType) =>
          Tagged(x, HostTag(ns, x)).toSeq
        case DataRecord(_, Some(key), x: XNamedGroup)  =>
          processGroup(KeyedGroup(key, x))(HostTag(ns, x), s)
        case DataRecord(_, _, x: XNamedAttributeGroup) =>
          processAttributeGroup(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelElement)     =>
          processTopLevelElement(x)(HostTag(ns, x), s)
        case DataRecord(_, _, x: XTopLevelAttribute)   =>
          processAttribute(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XNotation)            => Nil
      }
    }
  }

  def processSimpleType(decl: XSimpleType)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(decl, tag).toSeq ++
    (decl.arg1 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.arg1.simpleType map { processSimpleType } getOrElse {Nil}
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { processSimpleType } getOrElse {Nil}
      case DataRecord(_, _, x: XUnion) => Nil
    })

  def processGroup(group: KeyedGroup)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] = {
    // all, choice, and sequence are XExplicitGroupable, which are XGroup.
    // <xs:element name="element" type="xs:localElement"/>
    // <xs:element name="group" type="xs:groupRef"/>
    // <xs:element ref="xs:all"/>
    // <xs:element ref="xs:choice"/>
    // <xs:element ref="xs:sequence"/>
    // <xs:element ref="xs:any"/>
    def processParticle(particleKey: String, particle: XParticleOption) =
      particle match {
        case x: XLocalElementable  => processLocalElement(x)
        case x: XGroupRef          => processGroup(KeyedGroup(particleKey, x))
        case x: XExplicitGroupable => processGroup(KeyedGroup(particleKey, x))
        case x: XAny               => Nil
      }

    toThat(group, tag).toSeq ++
    (group.group.arg1.toSeq.flatMap {
      case DataRecord(_, Some(particleKey), x: XParticleOption) => processParticle(particleKey, x)
      case _ => Nil
    })
  }

  def processAttributeGroup(group: XAttributeGroup)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(group, tag).toSeq ++
    processAttrSeq(group.arg1)

  def processTopLevelElement(elem: XTopLevelElement)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] =
    toThat(elem, tag).toSeq ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => Tagged(x, tag).toSeq
      case x: XLocalSimpleType  => processSimpleType(x)
    }} getOrElse {Nil})

  private def processLocalElement(elem: XLocalElementable)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] =
    toThat(elem, schema.elementFormDefault, tag).toSeq ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => Tagged(x, tag).toSeq
      case x: XLocalSimpleType  => processSimpleType(x)
    }} getOrElse {Nil})

  def processAttribute(attr: XAttributable)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(attr, tag).toSeq ++
    (attr.simpleType map { processSimpleType } getOrElse {Nil})

  def processAnyAttribute(anyAttribute: XWildcardable)(implicit tag: HostTag): Seq[Tagged[_]] =
    Seq(TaggedAnyAttribute(anyAttribute, tag))

  def processAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): Seq[Tagged[_]] =
    (attrSeq.xattrdeclsoption1 flatMap {
      case DataRecord(_, _, x: XAttributable)      => processAttribute(x)
      case DataRecord(_, _, x: XAttributeGroupRef) => processAttributeGroup(x)
    }) ++
    (attrSeq.anyAttribute map {processAnyAttribute} getOrElse {Nil})
}

case class ComplexTypeOps(decl: Tagged[XComplexType]) {
  def particles(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToParticles(decl)

  lazy val primaryCompositor: Option[Tagged[KeyedGroup]] = ComplexTypeIteration.primaryCompositor(decl)
  lazy val primarySequence: Option[Tagged[KeyedGroup]] = ComplexTypeIteration.primarySequence(decl)
  lazy val primaryChoice: Option[Tagged[KeyedGroup]] = ComplexTypeIteration.primaryChoice(decl)
  lazy val primaryAll: Option[Tagged[KeyedGroup]] = ComplexTypeIteration.primaryAll(decl)

  def compositors(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToCompositors(decl)

  def flattenedCompositors(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToFlattenedCompositors(decl)

  def flattenedGroups(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToFlattenedGroups(decl)

  def flattenedAttributes(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToMergedAttributes(decl)

  def attributeGroups(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToAttributeGroups(decl)
}

class ComplexTypeIteration(underlying: Seq[Tagged[_]]) extends scala.collection.IndexedSeqLike[Tagged[_], ComplexTypeIteration] {
  lazy val length: Int = seq.length
  def apply(index: Int): Tagged[_] = seq(index)

  override def isEmpty = underlying.isEmpty
  override def toSeq = underlying.toSeq
  override def seq = underlying.seq
  override def iterator = underlying.iterator
  override protected[this] def newBuilder: Builder[Tagged[_], ComplexTypeIteration] = ComplexTypeIteration.newBuilder
}

object ComplexTypeIteration {
  import Defs._

  def apply(decl: Tagged[XComplexType])(implicit schema: XSchema): ComplexTypeIteration =
    new ComplexTypeIteration(complexTypeToSeq(decl))
  def fromSeq(seq: Seq[Tagged[_]]): ComplexTypeIteration = new ComplexTypeIteration(seq)

  def newBuilder: Builder[Tagged[_], ComplexTypeIteration] =
    new ArrayBuffer[Tagged[_]] mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[ComplexTypeIteration, Tagged[_], ComplexTypeIteration] =
    new CanBuildFrom[ComplexTypeIteration, Tagged[_], ComplexTypeIteration] {
      def apply(): Builder[Tagged[_], ComplexTypeIteration] = newBuilder
      def apply(from: ComplexTypeIteration): Builder[Tagged[_], ComplexTypeIteration] = newBuilder
    }

  def complexTypeToSeq(decl: Tagged[XComplexType])(implicit schema: XSchema): Seq[Tagged[_]] = {
    implicit val tag = decl.tag

    // <xs:group ref="xs:typeDefParticle"/>
    // <xs:group ref="xs:simpleRestrictionModel"/>
    def processRestriction(restriction: XRestrictionTypable) =
      (restriction.xrestrictiontypableoption map { _ match {
        case DataRecord(_, _, XSimpleRestrictionModelSequence(Some(simpleType), _)) =>
          SchemaIteration.processSimpleType(simpleType)
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, Some(key), x: XGroupRef)          => SchemaIteration.processGroup(KeyedGroup(key, x))
        case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaIteration.processGroup(KeyedGroup(key, x))
        case _ => Nil
      }} getOrElse {Nil}) ++ SchemaIteration.processAttrSeq(restriction.arg2)

    def processExtension(extension: XExtensionTypable) =
      (extension.arg1 map {
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, Some(key), x: XGroupRef)          => SchemaIteration.processGroup(KeyedGroup(key, x))
        case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaIteration.processGroup(KeyedGroup(key, x))
        case _ => Nil
      } getOrElse {Nil}) ++ SchemaIteration.processAttrSeq(extension.arg2)

    Seq(decl) ++
    (decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) =>
        (arg1 map {
          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => SchemaIteration.processGroup(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaIteration.processGroup(KeyedGroup(key, x))
          case _ => Nil
        } getOrElse {Nil}) ++ SchemaIteration.processAttrSeq(arg2)
    })
  }

  /** particles of the given decl flattened one level.
   * returns list of Tagged[XSimpleType], Tagged[BuiltInSimpleTypeSymbol], Tagged[XElement], Tagged[KeyedGroup],
   * Tagged[XAny].
   * <ul><li>local elements return <code>Tagged[XElement]</code></li>
   * <li>group references return <code>Tagged[KeyedGroup]</code></li>
   * <li>compositors also return <code>Tagged[KeyedGroup]</code></li>
   * <li>xs:any return <code>Tagged[XAny]</code></li>
   * <li>if the base is a builtin type, it will always return <code>Tagged[BuiltInSimpleTypeSymbol]</code></li>
   * <li>if the base is a simple type, it will always return <code>Tagged[XSimpleType]</code></li>
   */
  def complexTypeToParticles(decl: Tagged[XComplexType])
    (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[_]] = {
    import lookup._
    implicit val tag = decl.tag

    def toParticles(group: KeyedGroup): Seq[Tagged[_]] =
      if (group.key == "sequence" && Occurrence(group).isSingle) group.particles
      else Seq(Tagged(group, tag))

    def processRestriction(restriction: XRestrictionTypable) = {
      val base: QualifiedName = restriction.base
      base match {
        case BuiltInType(tagged) => Seq(tagged)
        case SimpleType(tagged)  => Seq(tagged)

        // if base is a complex type, keep the same for inheritance, otherwise it should be anyType
        case ComplexType(tagged) => complexTypeToParticles(tagged)

        // restriction of anyType
        case _ => restriction.xrestrictiontypableoption map { _ match {
          // see http://www.w3.org/TR/xmlschema-1/#Complex_Type_Definitions for details.
          case DataRecord(_, _, x@XSimpleRestrictionModelSequence(_, _)) =>
            x.simpleType map { simpleType => Seq(Tagged(simpleType, tag)) } getOrElse {Nil}

          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
          case _ => Nil
        }} getOrElse {Nil}
      } // base match
    } // processRestriction

    def processExtension(extension: XExtensionTypable) =  {
      val base: QualifiedName = extension.base
      base match {
        case BuiltInType(tagged) => Seq(tagged)
        case SimpleType(tagged)  => Seq(tagged)
        case ComplexType(tagged) =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, Some(key), x: XGroupRef)          =>
              complexTypeToParticles(tagged) ++ toParticles(KeyedGroup(key, x))
            case DataRecord(_, Some(key), x: XExplicitGroupable) =>
              complexTypeToParticles(tagged) ++ toParticles(KeyedGroup(key, x))
            case _ => complexTypeToParticles(tagged)
          } getOrElse { complexTypeToParticles(tagged) }

        // extension of anyType.
        case _ =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
            case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
            case _ => Nil
          } getOrElse {Nil}
      } // base match
    } // processExtension

    decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) =>
        arg1 map {
          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
          case _ => Nil
        } getOrElse {Nil}
    }
  }

  def complexTypeToFlattenedGroups(decl: Tagged[XComplexType])
        (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[KeyedGroup]] =
    complexTypeToFlattenedCompositors(decl) collect {
      case x@TaggedKeyedGroup(g, tag) if g.key == GroupTag => x
    }

  def complexTypeToFlattenedCompositors(decl: Tagged[XComplexType])
      (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[KeyedGroup]] = {
    import lookup._
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, Some(key), x: XGroupRef))          =>
        implicit val tag = decl.tag
        val compositor = Tagged(KeyedGroup(key, x), decl.tag)
        Seq(compositor) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        implicit val tag = decl.tag
        val compositor = Tagged(KeyedGroup(key, x), decl.tag)
        Seq(compositor) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case _ => Nil
    }

    def qnameCompositors(base: QualifiedName): Seq[Tagged[KeyedGroup]] = base match {
      case ComplexType(tagged) => complexTypeToFlattenedCompositors(tagged)
      case _ => Nil
    }

    decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) =>
        qnameCompositors(x.base) ++ extract(x.xrestrictiontypableoption)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          =>
        qnameCompositors(x.base) ++ extract(x.arg1)
      case XSimpleContent(_, _, _, _)                                                => Nil
      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2)                                    => extract(arg1)
    }
  }

  def primarySequence(decl: Tagged[XComplexType]): Option[Tagged[KeyedGroup]] =
    primaryCompositor(decl) match {
      case x@Some(TaggedKeyedGroup(g, tag)) if g.key == SequenceTag => x
      case _ => None
    }

  def primaryChoice(decl: Tagged[XComplexType]): Option[Tagged[KeyedGroup]] =
      primaryCompositor(decl) match {
        case x@Some(TaggedKeyedGroup(g, tag)) if g.key == ChoiceTag => x
        case _ => None
      }

  def primaryAll(decl: Tagged[XComplexType]): Option[Tagged[KeyedGroup]] =
      primaryCompositor(decl) match {
        case x@Some(TaggedKeyedGroup(g, tag)) if g.key == AllTag => x
        case _ => None
      }

  def primaryCompositor(decl: Tagged[XComplexType]): Option[Tagged[KeyedGroup]] = {
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, Some(key), x: XGroupRef))          => Some(Tagged(KeyedGroup(key, x), decl.tag))
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) => Some(Tagged(KeyedGroup(key, x), decl.tag))
      case _ => None
    }

    decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) =>
        extract(x.xrestrictiontypableoption)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          =>
        extract(x.arg1)

      case XSimpleContent(_, _, _, _)                                                => None
      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2)                                    => extract(arg1)
    }
  }

  def complexTypeToCompositors(decl: Tagged[XComplexType])
                      (implicit lookup: Lookup,
                       targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[KeyedGroup]] =
    primarySequence(decl).toSeq ++
    decl.particles collect {
      case Compositor(compositor) => compositor
    }

  def complexTypeToAttributeGroups(decl: Tagged[XComplexType])
                      (implicit lookup: Lookup,
                       targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[_]] = {
    implicit val s = lookup.schema.unbound
    import lookup._
    (decl collect  {
      case x: TaggedAttributeGroup => x.ref map { resolveAttributeGroup(_) } getOrElse x
    }).toSeq
  }

  /** attributes of the given decl flattened one level.
   * returns list of Tagged[XAttributable], Tagged[XAttributeGroup], Tagged[XWildCardable].
   */
  def complexTypeToMergedAttributes(decl: Tagged[XComplexType])
                      (implicit lookup: Lookup,
                       targetNamespace: Option[URI], scope: NamespaceBinding): Seq[Tagged[_]] = {
    import lookup._
    implicit val tag = decl.tag

    def qnameAttributes(base: QualifiedName) = base match {
      case ComplexType(tagged) => complexTypeToMergedAttributes(tagged)
      case _ => Nil
    }
    
    def isSameAttribute(lhs: Tagged[_], rhs: Tagged[_]): Boolean = {
      (lhs, rhs) match {
        case (l: TaggedAnyAttribute, r: TaggedAnyAttribute) => true
        case (l: TaggedAttribute, r: TaggedAttribute) =>
          QualifiedName(decl.tag.namespace, l.value.name, l.value.ref) ==
          QualifiedName(decl.tag.namespace, r.value.name, r.value.ref)
        case (l: TaggedAttributeGroup, r: TaggedAttributeGroup) =>
          QualifiedName(decl.tag.namespace, l.value.name, l.value.ref) ==
          QualifiedName(decl.tag.namespace, r.value.name, r.value.ref)
        case _ => false
      }
    }

    // since OO's hierarchy does not allow base members to be omitted,
    // child overrides needs to be implemented some other way.
    @tailrec def mergeAttributeSeqs(base: Seq[Tagged[_]], children: Seq[Tagged[_]]): Seq[Tagged[_]] = {
      def mergeAttribute(base: Seq[Tagged[_]], child: Tagged[_]): Seq[Tagged[_]] =
        if (base exists { x => isSameAttribute(x, child) }) base
        else base :+ child

      children match {
        case x :: xs => mergeAttributeSeqs(mergeAttribute(base, x), xs)
        case Nil => base
      }
    }

    def processRestriction(restriction: XRestrictionTypable) =
      mergeAttributeSeqs(qnameAttributes(restriction.base),
        flattenAttrSeq(restriction.arg2))

    def processExtension(extension: XExtensionTypable) =
      mergeAttributeSeqs(qnameAttributes(extension.base),
        flattenAttrSeq(extension.arg2))

    // move anyAttribute to the end.
    def reorderAttributes(xs: Seq[Tagged[_]]): Seq[Tagged[_]] = {
      val (l, r) = xs partition {
        case x: TaggedAnyAttribute => true
        case _ => false
      }
      r ++ l
    }

    // Resolve references as walking through the attributes.
    def flattenAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): Seq[Tagged[_]] =
      (attrSeq.xattrdeclsoption1 flatMap {
        case DataRecord(_, _, x: XAttributable)      =>
          x.ref map { ref => Seq(resolveAttribute(ref))
          } getOrElse { Seq(Tagged(x, tag)) }
        case DataRecord(_, _, x: XAttributeGroupRef) =>
          x.ref map { ref => flattenAttrSeq(resolveAttributeGroup(ref).value.arg1)
          } getOrElse { flattenAttrSeq(x.arg1) }
      }) ++
      (attrSeq.anyAttribute map {SchemaIteration.processAnyAttribute} getOrElse {Nil})

    val retval = decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) =>
        flattenAttrSeq(arg2)
    }
    reorderAttributes(retval)
  }
}

object Compositor {
  def unapply(value: Tagged[_]): Option[TaggedKeyedGroup] = value match {
    case x: TaggedKeyedGroup if List("choice", "all", "sequence") contains x.value.key => Some(x)
    case _ => None
  }
}

class ElementOps(val tagged: Tagged[XElement]) {
  def resolve(implicit lookup: Lookup): Tagged[XElement] = {
    import lookup._
    tagged.value.ref match {
      case Some(Element(x)) => x
      case _ => tagged
    }
  }

  def typeStructure(implicit lookup: Lookup): Tagged[_] = {
    import lookup._
    val elem = resolve.value

    // http://www.w3.org/TR/xmlschema-1/#declare-element
    // An <element> with no referenced or included type definition will correspond to an element declaration which
    // has the same type definition as the head of its substitution group if it identifies one, otherwise the
    // **ur-type definition**.
    val typeValue = elem.typeValue map { resolveType(_) }
    val localType = elem.xelementoption map { _ match {
      case DataRecord(_, _, x: XLocalSimpleType)  => Tagged(x, tagged.tag)
      case DataRecord(_, _, x: XLocalComplexType) => Tagged(x, tagged.tag)
    }}
    typeValue getOrElse {
      localType getOrElse { BuiltInAnyType.tagged }
    }
  }

  def qualified: Boolean = tagged match {
    case TaggedTopLevelElement(_, _) => true
    case elem: TaggedLocalElement =>
      elem.value.form map {_ == XQualified} getOrElse {elem.elementFormDefault == XQualified}
  }

  // @todo implement this
  def isSubstitutionGroup: Boolean = false
}

class AttributeGroupOps(val tagged: Tagged[XAttributeGroup]) {
  def flattenedAttributes: Seq[Tagged[_]] = SchemaIteration.processAttrSeq(tagged.value.arg1)(tagged.tag)
}

