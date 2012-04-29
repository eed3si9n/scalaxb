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

import scalashim._
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
  implicit def namedGroupToNamedGroupOps(tagged: Tagged[XNamedGroup]): NamedGroupOps = NamedGroupOps(tagged)
  implicit def keyedGroupToKeyedGroupOps(tagged: TaggedParticle[KeyedGroup]): KeyedGroupOps = KeyedGroupOps(tagged)
  
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
}

abstract class TopLevelType
case object SimpleTypeHost extends  TopLevelType
case object ComplexTypeHost extends TopLevelType
case object NamedGroupHost extends TopLevelType
case object AttributeGroupHost extends TopLevelType
case object ElementHost extends TopLevelType
case object AttributeHost extends TopLevelType
case class HostTag(val namespace: Option[URI],
  val topLevel: TopLevelType,
  val name: String,
  val path: String) {
  
  def withPath(path: String): HostTag = this.copy(path = path)
  def withChildPath(s: String): HostTag =
    if (s == "") this
    else withPath(path + "/" + s)
  final def /(s: String) = withChildPath(s)
  // override def toString = "HostTag(%s, %s, %s, %d)" format(namespace.toString, topLevel.toString, name, pos)
}
object HostTag {
  // def apply(namespace: Option[URI], topLevel: TopLevelType, name: String, pos: Int): HostTag =
  //   new HostTag(namespace, topLevel, name, pos)
  def apply(namespace: Option[URI], topLevel: TopLevelType, name: String): HostTag =
    HostTag(namespace, topLevel, name, "/")
  def apply(namespace: Option[URI], elem: XTopLevelElement): HostTag =
    HostTag(namespace, ElementHost, elem.name getOrElse {sys.error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelSimpleType): HostTag =
    HostTag(namespace, SimpleTypeHost, decl.name getOrElse {sys.error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelComplexType): HostTag =
    HostTag(namespace, ComplexTypeHost, decl.name getOrElse {sys.error("name is required.")})
  def apply(namespace: Option[URI], attr: XTopLevelAttribute): HostTag =
    HostTag(namespace, AttributeHost, attr.name getOrElse {sys.error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedGroup): HostTag =
    HostTag(namespace, NamedGroupHost, group.name getOrElse {sys.error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedAttributeGroup): HostTag =
    HostTag(namespace, AttributeGroupHost, group.name getOrElse {sys.error("name is required.")})
}

trait GroupOps {
  // List of TaggedElement, TaggedKeyedGroup, or TaggedAny.
  def particles(implicit lookup: Lookup, splitter: Splitter): Seq[TaggedParticle[_]]
}

sealed trait CompositorKey
case object SequenceTag extends CompositorKey { override def toString = "sequence" }
case object ChoiceTag extends CompositorKey { override def toString = "choice" }
case object AllTag extends CompositorKey { override def toString = "all" }

object KeyedGroup {
  def apply(key: String, value: XExplicitGroupable, tag: HostTag)(implicit schema: XSchema): KeyedGroup =
    apply(key match {
      case "sequence" => SequenceTag
      case "choice"   => ChoiceTag
      case "all"      => AllTag
    }, value, tag)

  def apply(key: CompositorKey, value: XExplicitGroupable, tag: HostTag)(implicit schema: XSchema): KeyedGroup = {
    // all, choice, and sequence are XExplicitGroupable, which are XGroup.
    // <xs:element name="element" type="xs:localElement"/>
    // <xs:element name="group" type="xs:groupRef"/>
    // <xs:element ref="xs:all"/>
    // <xs:element ref="xs:choice"/>
    // <xs:element ref="xs:sequence"/>
    // <xs:element ref="xs:any"/>
    val particles: Seq[TaggedParticle[_]] = value.arg1.toSeq.zipWithIndex map {
      case (DataRecord(_, Some(_), x: XLocalElementable), i)   => Tagged(x, schema.elementFormDefault, tag / i.toString)
      case (DataRecord(_, Some(_), x: XGroupRef), i)           => Tagged(x, tag / i.toString)
      case (DataRecord(_, Some(_), x: XAny), i)                => Tagged(x, tag / i.toString)
      case (DataRecord(_, Some(pk), x: XExplicitGroupable), i) =>
        // recursive
        Tagged(KeyedGroup(pk, x, tag / i.toString), tag / i.toString)
    }

    KeyedGroup(key, value.annotation, particles,
      value.id, value.name, value.ref, value.minOccurs, value.maxOccurs, value.attributes)
  }
}

case class KeyedGroup(key: CompositorKey,
  annotation: Option[xmlschema.XAnnotation] = None,
  particles: Seq[TaggedParticle[_]],
  id: Option[String] = None,
  name: Option[String] = None,
  ref: Option[javax.xml.namespace.QName] = None,
  minOccurs: BigInt,
  maxOccurs: String,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map())

case class KeyedGroupOps(tagged: TaggedParticle[KeyedGroup]) extends GroupOps {
  def particles(implicit lookup: Lookup, splitter: Splitter): Seq[TaggedParticle[_]] = tagged.particles
}

case class NamedGroupOps(tagged: Tagged[XNamedGroup]) extends GroupOps {
  def value = tagged.value
  def tag = tagged.tag
  def particles(implicit lookup: Lookup, splitter: Splitter): Seq[TaggedParticle[_]] =
    ComplexTypeIteration.groupToParticles(value, tag)
}

/** represents attributes param */
case class AttributeSeqParam() {}

/** represents param created for xs:all. */
// case class AllParam() {}

sealed trait Tagged[+A] {
  def value: A
  def tag: HostTag
  override def toString: String = "Tagged(%s, %s)".format(value.toString, tag.toString)
}

sealed trait TaggedAttr[+A] extends Tagged[A] {}

object Tagged {
  def apply(value: XSimpleType, tag: HostTag): TaggedType[XSimpleType] = TaggedSimpleType(value, tag)
  def apply(value: XComplexType, tag: HostTag): TaggedType[XComplexType] = TaggedComplexType(value, tag)
  def apply(value: XNamedGroup, tag: HostTag): Tagged[XNamedGroup] = TaggedNamedGroup(value, tag)
  def apply(value: XGroupRef, tag: HostTag): TaggedParticle[XGroupRef] = TaggedGroupRef(value, tag)
  def apply(value: KeyedGroup, tag: HostTag): TaggedParticle[KeyedGroup] = TaggedKeyedGroup(value, tag)
  def apply(value: XAttributeGroup, tag: HostTag): TaggedAttr[XAttributeGroup] = TaggedAttributeGroup(value, tag)
  def apply(value: XTopLevelElement, tag: HostTag): Tagged[XTopLevelElement] = TaggedTopLevelElement(value, tag)
  def apply(value: XLocalElementable, elementFormDefault: XFormChoice, tag: HostTag): TaggedParticle[XLocalElementable] =
    TaggedLocalElement(value, elementFormDefault, tag)
  def apply(value: XAttributable, tag: HostTag): TaggedAttr[XAttributable] = TaggedAttribute(value, tag)
  def apply(value: XAny, tag: HostTag): TaggedParticle[XAny] = TaggedWildCard(value, tag)
  def apply(value: XsTypeSymbol, tag: HostTag): TaggedType[XsTypeSymbol] = TaggedSymbol(value, tag)
  def apply(value: XNoFixedFacet, tag: HostTag): Tagged[XNoFixedFacet] = TaggedEnum(value, tag)
  def apply(value: AttributeSeqParam, tag: HostTag): Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(value, tag)

  implicit def box(value: XSimpleType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XComplexType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XNamedGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttributeGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XTopLevelElement)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttributable)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XsTypeSymbol)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XNoFixedFacet)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: AttributeSeqParam)(implicit tag: HostTag) = Tagged(value, tag)
  
  implicit def unbox[A](tagged: Tagged[A]): A = tagged.value
}

// Tagged Particle
sealed trait TaggedParticle[+A] extends Tagged[A] {}
case class TaggedLocalElement(value: XLocalElementable, elementFormDefault: XFormChoice,
                              tag: HostTag) extends TaggedParticle[XLocalElementable] {}
case class TaggedGroupRef(value: XGroupRef, tag: HostTag) extends TaggedParticle[XGroupRef] {}
case class TaggedKeyedGroup(value: KeyedGroup, tag: HostTag) extends TaggedParticle[KeyedGroup] {}
case class TaggedWildCard(value: XAny, tag: HostTag) extends TaggedParticle[XAny] {}

sealed trait TaggedType[+A] extends Tagged[A] {}
case class TaggedSimpleType(value: XSimpleType, tag: HostTag) extends TaggedType[XSimpleType] {}
case class TaggedComplexType(value: XComplexType, tag: HostTag) extends TaggedType[XComplexType] {}
case class TaggedSymbol(value: XsTypeSymbol, tag: HostTag) extends TaggedType[XsTypeSymbol] {}
object TaggedXsAnyType extends TaggedSymbol(XsAnyType, HostTag(Some(Defs.SCALAXB_URI), SimpleTypeHost, "anyType"))

case class TaggedNamedGroup(value: XNamedGroup, tag: HostTag) extends Tagged[XNamedGroup] {}
case class TaggedAttributeGroup(value: XAttributeGroup, tag: HostTag) extends TaggedAttr[XAttributeGroup] {}
case class TaggedTopLevelElement(value: XTopLevelElement, tag: HostTag) extends Tagged[XTopLevelElement] {}
case class TaggedAttribute(value: XAttributable, tag: HostTag) extends TaggedAttr[XAttributable] {}
case class TaggedAnyAttribute(value: XWildcardable, tag: HostTag) extends TaggedAttr[XWildcardable] {}
case class TaggedEnum(value: XNoFixedFacet, tag: HostTag) extends Tagged[XNoFixedFacet] {}
case class TaggedDataRecordSymbol(value: DataRecordSymbol) extends Tagged[DataRecordSymbol] {
  import Defs._
  val tag = HostTag(Some(SCALAXB_URI), SimpleTypeHost, "DataRecord")
}
case class TaggedAttributeSeqParam(value: AttributeSeqParam, tag: HostTag) extends Tagged[AttributeSeqParam] {}

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
  def toThat(group: XNamedGroup, tag: HostTag): Option[Tagged[_]] = Some(Tagged(group, tag))
  def toThat(group: XGroupRef, tag: HostTag): Option[Tagged[_]] = Some(Tagged(group, tag))
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
          processTopLevelSimpleType(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelComplexType) =>
          processTopLevelComplexType(x)(HostTag(ns, x), s)
        case DataRecord(_, _, x: XNamedGroup)  =>
          processNamedGroup(x)(HostTag(ns, x), s)
        case DataRecord(_, _, x: XNamedAttributeGroup) =>
          processAttributeGroup(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelElement)     =>
          processTopLevelElement(x)(HostTag(ns, x), s)
        case DataRecord(_, _, x: XTopLevelAttribute)   =>
          processTopLevelAttribute(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XNotation)            => Nil
      }
    }
  }

  def processTopLevelComplexType(decl: XTopLevelComplexType)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] =
    ComplexTypeIteration.complexTypeToSeq(Tagged(decl, tag))

  def processLocalComplexType(decl: XLocalComplexType, childtag: HostTag)(implicit schema: XSchema): Seq[Tagged[_]] =
    ComplexTypeIteration.complexTypeToSeq(Tagged(decl, childtag))

  def processTopLevelSimpleType(decl: XTopLevelSimpleType)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(decl, tag).toSeq ++
    (decl.arg1 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.arg1.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse {Nil}
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse {Nil}
      case DataRecord(_, _, x: XUnion) => Nil
    })

  def processLocalSimpleType(decl: XSimpleType, childtag: HostTag): Seq[Tagged[_]] =
    toThat(decl, childtag).toSeq ++
    (decl.arg1 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.arg1.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse {Nil}
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse {Nil}
      case DataRecord(_, _, x: XUnion) => Nil
    })

  def processNamedGroup(group: XNamedGroup)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] =
    toThat(group, tag).toSeq ++
    (group.arg1.toSeq.zipWithIndex.flatMap {
      case (DataRecord(_, Some(particleKey), x: XParticleOption), i) =>
        ComplexTypeIteration.processGroupParticle(particleKey, x, tag / i.toString)
      case _ => Nil
    })

  def processAttributeGroup(group: XAttributeGroup)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(group, tag).toSeq ++
    processAttrSeq(group.arg1)

  def processTopLevelElement(elem: XTopLevelElement)(implicit tag: HostTag, schema: XSchema): Seq[Tagged[_]] =
    toThat(elem, tag).toSeq ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => processLocalComplexType(x, tag / "_")
      case x: XLocalSimpleType  => processLocalSimpleType(x, tag / "_")
    }} getOrElse {Nil})

  def processTopLevelAttribute(attr: XTopLevelAttribute)(implicit tag: HostTag): Seq[Tagged[_]] =
    toThat(attr, tag).toSeq ++
    (attr.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse {Nil})

  def processLocalAttribute(attr: XAttributable, childtag: HostTag): Seq[Tagged[_]] =
    toThat(attr, childtag).toSeq ++
    (attr.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse {Nil})

  def processAnyAttribute(anyAttribute: XWildcardable)(implicit tag: HostTag): Seq[Tagged[_]] =
    Seq(TaggedAnyAttribute(anyAttribute, tag))

  def processAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): Seq[Tagged[_]] =
    (attrSeq.xattrdeclsoption1.zipWithIndex flatMap {
      case (DataRecord(_, _, x: XAttributable), i)      => processLocalAttribute(x, tag / i.toString)
      case (DataRecord(_, _, x: XAttributeGroupRef), i) => processAttributeGroup(x)
    }) ++
    (attrSeq.anyAttribute map {processAnyAttribute} getOrElse {Nil})
}

case class ComplexTypeOps(decl: Tagged[XComplexType]) {
  def particles(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToParticles(decl)

  def primaryCompositor(implicit lookup: Lookup): Option[TaggedParticle[_]] = {
    implicit val unbound = lookup.schema.unbound
    ComplexTypeIteration.primaryCompositor(decl)
  } 
  def primarySequence(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] = {
    implicit val unbound = lookup.schema.unbound
    ComplexTypeIteration.primarySequence(decl)
  }
  def primaryChoice(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] = {
    implicit val unbound = lookup.schema.unbound
    ComplexTypeIteration.primaryChoice(decl)
  } 
  def primaryAll(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] = {
    implicit val unbound = lookup.schema.unbound
    ComplexTypeIteration.primaryAll(decl)
  }

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

  /** returns sequence of tagged objects from the given complex type. */
  def complexTypeToSeq(decl: Tagged[XComplexType])(implicit schema: XSchema): Seq[Tagged[_]] = {
    implicit val tag = decl.tag
    val childtag = tag / "_"

    // <xs:group ref="xs:typeDefParticle"/>
    // <xs:group ref="xs:simpleRestrictionModel"/>
    def processRestriction(restriction: XRestrictionTypable): Seq[Tagged[_]] =
      (restriction.xrestrictiontypableoption map { _ match {
        case DataRecord(_, _, XSimpleRestrictionModelSequence(Some(simpleType), _)) =>
          SchemaIteration.processLocalSimpleType(simpleType, childtag)
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
        case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroup(KeyedGroup(key, x, childtag), childtag)
        case _ => Nil
      }} getOrElse {Nil}) ++ SchemaIteration.processAttrSeq(restriction.arg2)

    def processExtension(extension: XExtensionTypable): Seq[Tagged[_]] =
      (extension.arg1 map {
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
        case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroup(KeyedGroup(key, x, childtag), childtag)
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
          case DataRecord(_, Some(key), x: XGroupRef)          => processGroupRef(x, tag)
          case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroup(KeyedGroup(key, x, childtag), childtag)
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
   * // <li>if the base is a builtin type, it will always return <code>Tagged[BuiltInSimpleTypeSymbol]</code></li>
   * // <li>if the base is a simple type, it will always return <code>Tagged[XSimpleType]</code></li>
   */
  def complexTypeToParticles(decl: Tagged[XComplexType])
    (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[TaggedParticle[_]] = {
    import lookup._
    implicit val tag = decl.tag
    val childtag = tag / "_"
    implicit val unbound = schema.unbound

    def processRestriction(restriction: XRestrictionTypable): Seq[TaggedParticle[_]] = {
      val base: QualifiedName = restriction.base
      base match {
        // case BuiltInType(tagged) => Seq(tagged)
        // case SimpleType(tagged)  => Seq(tagged)

        // if base is a complex type, keep the same for inheritance, otherwise it should be anyType
        case ComplexType(tagged) => complexTypeToParticles(tagged)

        // restriction of anyType
        case _ => restriction.xrestrictiontypableoption map { _ match {
          // see http://www.w3.org/TR/xmlschema-1/#Complex_Type_Definitions for details.
          //case DataRecord(_, _, x@XSimpleRestrictionModelSequence(_, _)) =>
          //  x.simpleType map { simpleType => Seq(Tagged(simpleType, tag)) } getOrElse {Nil}

          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
          case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
          case _ => Nil
        }} getOrElse {Nil}
      } // base match
    } // processRestriction

    def processExtension(extension: XExtensionTypable): Seq[TaggedParticle[_]] =  {
      val base: QualifiedName = extension.base
      base match {
        // case BuiltInType(tagged) => Seq(tagged)
        // case SimpleType(tagged)  => Seq(tagged)
        case ComplexType(tagged) =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, _, x: XGroupRef)          =>
              complexTypeToParticles(tagged) ++ processGroupRef(x, childtag)
            case DataRecord(_, Some(key), x: XExplicitGroupable) =>
              complexTypeToParticles(tagged) ++ processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
            case _ => complexTypeToParticles(tagged)
          } getOrElse { complexTypeToParticles(tagged) }

        // extension of anyType.
        case _ =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, _, x: XGroupRef)                  => processGroupRef(x, childtag)
            case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
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
          case DataRecord(_, _, x: XGroupRef)          => processGroupRef(x, childtag)
          case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
          case _ => Nil
        } getOrElse {Nil}
    }
  }

  private[scalaxb] def processGroupRef(ref: XGroupRef, childtag: HostTag): Seq[TaggedParticle[_]] =
    Seq(Tagged(ref, childtag))

  private[scalaxb] def processKeyedGroup(group: KeyedGroup, childtag: HostTag)(implicit schema: XSchema): Seq[Tagged[_]] =
    Seq(Tagged(group, childtag)) ++
    (group.particles flatMap {
      case TaggedLocalElement(x, form, tag) => processLocalElement(x, tag)
      case TaggedGroupRef(x, tag)     => processGroupRef(x, tag)
      case TaggedKeyedGroup(x, tag)   => processKeyedGroup(x, tag)
      case TaggedWildCard(x, tag)     => Nil
    }) 

  // all, choice, and sequence are XExplicitGroupable, which are XGroup.
  // <xs:element name="element" type="xs:localElement"/>
  // <xs:element name="group" type="xs:groupRef"/>
  // <xs:element ref="xs:all"/>
  // <xs:element ref="xs:choice"/>
  // <xs:element ref="xs:sequence"/>
  // <xs:element ref="xs:any"/>
  private[scalaxb] def processGroupParticle(particleKey: String, particle: XParticleOption, childtag: HostTag)
                                  (implicit schema: XSchema): Seq[Tagged[_]] =
    particle match {
      case x: XLocalElementable  => processLocalElement(x, childtag)
      case x: XGroupRef          => processGroupRef(x, childtag)
      case x: XExplicitGroupable => processKeyedGroup(KeyedGroup(particleKey, x, childtag), childtag)
      case x: XAny               => Nil
    }

  private[scalaxb] def processLocalElement(elem: XLocalElementable, childtag: HostTag)(implicit schema: XSchema): Seq[Tagged[_]] =
    Seq(Tagged(elem, schema.elementFormDefault, childtag)) ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => SchemaIteration.processLocalComplexType(x, childtag / "_")
      case x: XLocalSimpleType  => SchemaIteration.processLocalSimpleType(x, childtag / "_")
    }} getOrElse {Nil})

  private[scalaxb] def processKeyedGroupParticle(group: KeyedGroup, childtag: HostTag)
      (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[TaggedParticle[_]] =
    if ((group.key == SequenceTag) && Occurrence(group).isSingle) group.particles
    else Seq(Tagged(group, childtag))

  // List of TaggedElement, TaggedKeyedGroup, or TaggedAny.
  def groupToParticles(value :XGroup, tag: HostTag)(implicit lookup: Lookup, splitter: Splitter): Seq[TaggedParticle[_]] =
    value.arg1.toSeq.zipWithIndex flatMap {
      case (DataRecord(_, _, x: XLocalElementable), i) =>
        Seq(TaggedLocalElement(x, lookup.schema.unbound.elementFormDefault, tag / i.toString))
      case (DataRecord(_, _, x: XGroupRef), i) =>
        Seq(Tagged(x, tag / i.toString))
      case (DataRecord(_, Some("sequence"), x: XExplicitGroupable), i)  =>
        implicit val unbound = lookup.schema.unbound
        innerSequenceToParticles(Tagged(KeyedGroup(SequenceTag, x, tag / i.toString), tag / i.toString))
      case (DataRecord(_, Some(particleKey), x: XExplicitGroupable), i) =>
        implicit val unbound = lookup.schema.unbound
        Seq(Tagged(KeyedGroup(particleKey, x, tag / i.toString), tag / i.toString))
      case (DataRecord(_, _, x: XAny), i) => Seq(Tagged(x, tag / i.toString))
    }

  private[scalaxb] def innerSequenceToParticles(tagged: TaggedParticle[KeyedGroup])
      (implicit lookup: Lookup, splitter: Splitter): Seq[TaggedParticle[_]] =
    if (tagged.value.minOccurs != 1 || tagged.value.maxOccurs != "1")
      if (tagged.value.particles.length == 1) tagged.value.particles(0) match {
        case TaggedWildCard(any, tag) =>
          Seq(Tagged(any.copy(
            minOccurs = math.min(any.minOccurs.toInt, tagged.value.minOccurs.toInt),
            maxOccurs = Occurrence.max(any.maxOccurs, tagged.value.maxOccurs)
          ), tag))
        case TaggedKeyedGroup(value, tag) if value.key == ChoiceTag =>
          Seq(Tagged(value.copy(
            minOccurs = math.min(value.minOccurs.toInt, tagged.value.minOccurs.toInt),
            maxOccurs = Occurrence.max(value.maxOccurs, tagged.value.maxOccurs)
          ), tag))
        case _ => Seq(tagged)
      }
      else Seq(tagged)
    else splitter.splitIfLongSequence(tagged)

  def complexTypeToFlattenedGroups(decl: Tagged[XComplexType])
        (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[TaggedParticle[XGroupRef]] =
    complexTypeToFlattenedCompositors(decl) collect {
      case x: TaggedGroupRef => x
    }

  def complexTypeToFlattenedCompositors(decl: Tagged[XComplexType])
      (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Seq[TaggedParticle[_]] = {
    import lookup._
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, _, x: XGroupRef))          =>
        implicit val tag = decl.tag
        val childtag = tag / "_"
        val compositor = Tagged(resolveNamedGroup(x.ref.get), childtag)
        val tagged = Tagged(x, childtag)
        Seq(tagged) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        implicit val tag = decl.tag
        implicit val unbound = schema.unbound
        val childtag = tag / "_"
        val compositor = Tagged(KeyedGroup(key, x, childtag), childtag)
        Seq(compositor) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case _ => Nil
    }

    def qnameCompositors(base: QualifiedName): Seq[TaggedParticle[_]] = base match {
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

  def primarySequence(decl: Tagged[XComplexType])(implicit schema: XSchema): Option[TaggedParticle[KeyedGroup]] =
    primaryCompositor(decl) flatMap { _ match {
      case x: TaggedKeyedGroup if x.value.key == SequenceTag => Some(x)
      case _ => None
    }}

  def primaryChoice(decl: Tagged[XComplexType])(implicit schema: XSchema): Option[TaggedParticle[KeyedGroup]] =
      primaryCompositor(decl) flatMap { _ match {
        case x: TaggedKeyedGroup if x.value.key == ChoiceTag => Some(x)
        case _ => None
      }}

  def primaryAll(decl: Tagged[XComplexType])(implicit schema: XSchema): Option[TaggedParticle[KeyedGroup]] =
      primaryCompositor(decl) flatMap { _ match {
        case x: TaggedKeyedGroup if x.value.key == AllTag => Some(x)
        case _ => None
      }}

  def primaryCompositor(decl: Tagged[XComplexType])(implicit schema: XSchema): Option[TaggedParticle[_]] = {
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, Some(key), x: XGroupRef))          => Some(Tagged(x, decl.tag / "_"))
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        Some(Tagged(KeyedGroup(key, x, decl.tag / "_"), decl.tag / "_"))
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
                       targetNamespace: Option[URI], scope: NamespaceBinding): Seq[TaggedParticle[KeyedGroup]] = {
    implicit val unbound = lookup.schema.unbound
    val ps = primarySequence(decl)

    // the primary sequence could appear as particles if it has multiple occurrence
    ps.toSeq ++
    (decl.particles collect {
      case Compositor(compositor) if Some(compositor) != ps => compositor
    })
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
  import Defs._
  def unapply(value: Tagged[_]): Option[TaggedKeyedGroup] = value match {
    case x: TaggedKeyedGroup => Some(x)
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

  def localType(implicit lookup: Lookup): Option[TaggedType[_]] = {
    import lookup._
    val elem = resolve.value
    val typeValue = elem.typeValue map { resolveType(_) }
    typeValue match {
      case Some(x) => None
      case None => getLocalType
    }
  }

  private[this] def getLocalType(implicit lookup: Lookup): Option[TaggedType[_]] = {
    import lookup._
    val elem = resolve.value
    elem.xelementoption map { _ match {
      case DataRecord(_, _, x: XLocalSimpleType)  => Tagged(x, tagged.tag / "_")
      case DataRecord(_, _, x: XLocalComplexType) => Tagged(x, tagged.tag / "_")
    }}
  }

  def typeStructure(implicit lookup: Lookup): TaggedType[_] = {
    import lookup._
    val elem = resolve.value

    // http://www.w3.org/TR/xmlschema-1/#declare-element
    // An <element> with no referenced or included type definition will correspond to an element declaration which
    // has the same type definition as the head of its substitution group if it identifies one, otherwise the
    // **ur-type definition**.
    val typeValue = elem.typeValue map { resolveType(_) }
    typeValue getOrElse {
      getLocalType getOrElse { TaggedXsAnyType }
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

