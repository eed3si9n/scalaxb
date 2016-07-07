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
import scalaxb.compiler.xsd.{XsTypeSymbol, XsAnyType, XsNillableAny, XsString}
import xmlschema._
import scala.collection.mutable.{Builder, ArrayBuffer}
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import masked.scalaxb.DataRecord

object Defs {
  implicit def schemaToSchemaIteration(schema: XSchema): SchemaIteration = SchemaIteration(schema)
  implicit def complexTypeToComplexTypeOps(tagged: TaggedType[XComplexType]): ComplexTypeOps = ComplexTypeOps(tagged)
  implicit def complexTypeToComplexTypeIteration(tagged: TaggedType[XComplexType])(implicit schema: XSchema): ComplexTypeIteration =
    ComplexTypeIteration(tagged)
  implicit def elementToElementOps(tagged: Tagged[XElement]): ElementOps = new ElementOps(tagged)
  implicit def attributeGroupToAttributeGroupOps(tagged: TaggedAttr[XAttributeGroup]): AttributeGroupOps =
    new AttributeGroupOps(tagged)
  implicit def namedGroupToNamedGroupOps(tagged: Tagged[XNamedGroup]): NamedGroupOps = NamedGroupOps(tagged)
  implicit def simpleTypeToSimpleTypeOps(tagged: TaggedType[XSimpleType]): SimpleTypeOps = SimpleTypeOps(tagged)
  implicit def attributeToAttributeOps(tagged: TaggedAttr[XAttributable]): AttributeOps = AttributeOps(tagged)

  val XML_SCHEMA_URI = new URI("http://www.w3.org/2001/XMLSchema")
  val XS_PREFIX = "xs"
  val XSI_URL = new URI("http://www.w3.org/2001/XMLSchema-instance")
  val XSI_PREFIX = "xsi"
  val XML_URI = new URI("http://www.w3.org/XML/1998/namespace")
  val XML_PREFIX = "xml"
  val SCALA_URI = new URI("http://scala-lang.org/")
  val SCALAXB_URI = new URI("http://scalaxb.org/")
  val NL = System.getProperty("line.separator")

  val XS_ANY_TYPE = QualifiedName(XML_SCHEMA_URI, "anyType")
  val XS_ANY_SIMPLE_TYPE = QualifiedName(XML_SCHEMA_URI, "anySimpleType")
  val XS_STRING_TYPE = QualifiedName(XML_SCHEMA_URI, "string")
  val XS_ID_TYPE = QualifiedName(XML_SCHEMA_URI, "ID")
  val XS_ANYURI_TYPE = QualifiedName(XML_SCHEMA_URI, "anyURI") 
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
  def particles(implicit lookup: Lookup, splitter: Splitter): IndexedSeq[TaggedParticle[_]]
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
    val particles: Vector[TaggedParticle[_]] = Vector(value.xParticleOption3: _*).zipWithIndex map {
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
  particles: Vector[TaggedParticle[_]],
  id: Option[String] = None,
  name: Option[String] = None,
  ref: Option[javax.xml.namespace.QName] = None,
  minOccurs: BigInt,
  maxOccurs: String,
  attributes: Map[String, DataRecord[Any]] = Map())

case class NamedGroupOps(tagged: Tagged[XNamedGroup]) extends GroupOps {
  def value = tagged.value
  def tag = tagged.tag
  def particles(implicit lookup: Lookup, splitter: Splitter): IndexedSeq[TaggedParticle[_]] =
    ComplexTypeIteration.namedGroupToParticles(value, tag)
  def primaryCompositor(implicit lookup: Lookup, splitter: Splitter) =
    ComplexTypeIteration.namedGroupToPrimaryCompositor(value, tag)
  def compositors(implicit lookup: Lookup, splitter: Splitter) =
    ComplexTypeIteration.namedGroupToCompositors(value, tag)   
}

case class SimpleTypeOps(decl: TaggedType[XSimpleType]) {
  def baseType(implicit lookup: Lookup): TaggedType[_] = {
    import Defs._
    import lookup.{BuiltInType, SimpleType}
    decl.value.xSimpleDerivationOption3.value match {
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => tagged
          case SimpleType(tagged)  => tagged
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        Tagged(simpleType, decl.tag)
      case XList(_, _, _, Some(itemType), _) =>
        QualifiedName(itemType) match {
          case BuiltInType(tagged) => tagged
          case SimpleType(tagged)  => tagged
        }
      case XList(_, Some(simpleType), _, _, _) =>
        Tagged(simpleType, decl.tag)
      case x: XUnion => TaggedXsString
      case _ => sys.error("baseType#: Unsupported content " + decl.xSimpleDerivationOption3.value.toString)
    }    
  }
  /** this decl is a decendant of QName. */
  def qnameBased(implicit lookup: Lookup): Boolean = {
    import Defs._
    import lookup.{BuiltInType, SimpleType}
    import scalaxb.compiler.xsd.XsQName
    decl.value.xSimpleDerivationOption3.value match {
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(TaggedSymbol(XsQName, _)) => true
          case SimpleType(tagged)                    => SimpleTypeOps(tagged).qnameBased
          case _ => false
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        SimpleTypeOps(Tagged(simpleType, decl.tag)).qnameBased
      case _ => false
    }    
  }

  def enumValue(enum: Tagged[XNoFixedFacet])(implicit lookup: Lookup): Any =
    if (qnameBased) {
      import scalaxb.compiler.Module.splitTypeName
      val (ns, localPart) = splitTypeName(enum.value.value, lookup.scopeEv)
      QualifiedName(ns map {new URI(_)}, localPart)
    }
    else enum.value.value
}

/** represents attributes param */
case class AttributeSeqParam() {}

/** represents mixed param. */
case class MixedSeqParam() {}

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
  def apply(value: XTopLevelAttribute, tag: HostTag): TaggedAttr[XTopLevelAttribute] = TaggedTopLevelAttribute(value, tag)
  def apply(value: XAttribute, tag: HostTag): TaggedAttr[XAttribute] = TaggedLocalAttribute(value, tag)
  def apply(value: XAny, tag: HostTag): TaggedParticle[XAny] = TaggedWildCard(value, tag)
  def apply(value: XsTypeSymbol, tag: HostTag): TaggedType[XsTypeSymbol] = TaggedSymbol(value, tag)
  def apply(value: XNoFixedFacet, tag: HostTag): Tagged[XNoFixedFacet] = TaggedEnum(value, tag)
  def apply(value: AttributeSeqParam, tag: HostTag): Tagged[AttributeSeqParam] = TaggedAttributeSeqParam(value, tag)
  def apply(value: MixedSeqParam, tag: HostTag): Tagged[MixedSeqParam] = TaggedMixedSeqParam(value, tag)
    
  implicit def box(value: XSimpleType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XComplexType)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XNamedGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttributeGroup)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XTopLevelElement)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XTopLevelAttribute)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XAttribute)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XsTypeSymbol)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: XNoFixedFacet)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: AttributeSeqParam)(implicit tag: HostTag) = Tagged(value, tag)
  implicit def box(value: MixedSeqParam)(implicit tag: HostTag) = Tagged(value, tag)
  
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
object TaggedXsNillableAny extends TaggedSymbol(XsNillableAny, HostTag(Some(Defs.SCALAXB_URI), SimpleTypeHost, "nillableAny"))
object TaggedXsString extends TaggedSymbol(XsString, HostTag(Some(Defs.XML_SCHEMA_URI), SimpleTypeHost, "string"))

case class TaggedTopLevelElement(value: XTopLevelElement, tag: HostTag) extends Tagged[XTopLevelElement] {}
case class TaggedNamedGroup(value: XNamedGroup, tag: HostTag) extends Tagged[XNamedGroup] {}
case class TaggedAttributeGroup(value: XAttributeGroup, tag: HostTag) extends TaggedAttr[XAttributeGroup] {}
case class TaggedTopLevelAttribute(value: XTopLevelAttribute, tag: HostTag) extends TaggedAttr[XTopLevelAttribute] {}
case class TaggedLocalAttribute(value: XAttribute, tag: HostTag) extends TaggedAttr[XAttribute] {}
case class TaggedAnyAttribute(value: XWildcardable, tag: HostTag) extends TaggedAttr[XWildcardable] {}
case class TaggedEnum(value: XNoFixedFacet, tag: HostTag) extends Tagged[XNoFixedFacet] {}
case class TaggedDataRecordSymbol(value: DataRecordSymbol) extends Tagged[DataRecordSymbol] {
  import Defs._
  val tag = HostTag(Some(SCALAXB_URI), SimpleTypeHost, "DataRecord")
}
case class TaggedOptionSymbol(value: OptionSymbol) extends Tagged[OptionSymbol] {
  import Defs._
  val tag = HostTag(Some(SCALA_URI), SimpleTypeHost, "Option")
}
case class DataRecordSymbol(member: Tagged[Any]) extends XsTypeSymbol {
  val name = "DataRecordSymbol(" + member + ")"
}
case class OptionSymbol(member: Tagged[Any]) extends XsTypeSymbol {
  val name = "OptionSymbol(" + member + ")"
}
case class TaggedAttributeSeqParam(value: AttributeSeqParam, tag: HostTag) extends Tagged[AttributeSeqParam] {}
case class TaggedMixedSeqParam(value: MixedSeqParam, tag: HostTag) extends Tagged[MixedSeqParam] {}

class SchemaIteration(underlying: IndexedSeq[Tagged[_]]) extends scala.collection.IndexedSeqLike[Tagged[_], SchemaIteration] {
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
  def fromSeq(seq: IndexedSeq[Tagged[_]]): SchemaIteration = new SchemaIteration(seq)

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
  def toThat(group: XAttributeGroup, tag: HostTag): Option[TaggedAttr[_]] = Some(Tagged(group, tag))
  def toThat(elem: XTopLevelElement, tag: HostTag): Option[Tagged[_]] = Some(Tagged(elem, tag))
  def toThat(elem: XLocalElementable, elementFormDefault: XFormChoice, tag: HostTag): Option[Tagged[_]] =
    Some(Tagged(elem, elementFormDefault, tag))
  def toThat(attr: XTopLevelAttribute, tag: HostTag): Option[TaggedAttr[_]] = Some(Tagged(attr, tag))
  def toThat(attr: XAttribute, tag: HostTag): Option[TaggedAttr[_]] = Some(Tagged(attr, tag))

  def schemaToSeq(schema: XSchema): IndexedSeq[Tagged[_]] = {
    implicit val s = schema
    val ns = schema.targetNamespace

    // <xs:element ref="xs:simpleType"/>
    // <xs:element ref="xs:complexType"/>
    // <xs:element ref="xs:group"/>
    // <xs:element ref="xs:attributeGroup"/>
    // <xs:element ref="xs:element"/>
    // <xs:element ref="xs:attribute"/>
    // <xs:element ref="xs:notation"/>
    Vector(schema.xschemasequence1: _*) flatMap {
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
        case DataRecord(_, _, x: XNotation)            => Vector()
      }
    }
  }

  def processTopLevelComplexType(decl: XTopLevelComplexType)(implicit tag: HostTag, schema: XSchema): IndexedSeq[Tagged[_]] =
    ComplexTypeIteration.complexTypeToSeq(Tagged(decl, tag))

  def processLocalComplexType(decl: XLocalComplexType, childtag: HostTag)(implicit schema: XSchema): IndexedSeq[Tagged[_]] =
    ComplexTypeIteration.complexTypeToSeq(Tagged(decl, childtag))

  def processTopLevelSimpleType(decl: XTopLevelSimpleType)(implicit tag: HostTag): IndexedSeq[Tagged[_]] =
    toThat(decl, tag).toIndexedSeq ++
    (decl.xSimpleDerivationOption3 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.xSimpleRestrictionModelSequence3.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse Vector()
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse Vector()
      case DataRecord(_, _, x: XUnion) => Vector()
    })

  def processLocalSimpleType(decl: XSimpleType, childtag: HostTag): IndexedSeq[Tagged[_]] =
    toThat(decl, childtag).toIndexedSeq ++
    (decl.xSimpleDerivationOption3 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.xSimpleRestrictionModelSequence3.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse Vector()
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse Vector()
      case DataRecord(_, _, x: XUnion) => Vector()
    })

  def processNamedGroup(group: XNamedGroup)(implicit tag: HostTag, schema: XSchema): IndexedSeq[Tagged[_]] =
    toThat(group, tag).toIndexedSeq ++
    (group.xParticleOption3.toSeq.headOption match {
      case Some(DataRecord(_, Some(particleKey), x)) =>
        x match {
          case compositor: XExplicitGroupable =>
            ComplexTypeIteration.processKeyedGroup(KeyedGroup(particleKey, compositor, tag / 0.toString), tag / 0.toString)
          case _ => Vector()
        }
      case _ => Vector()
    })

  def processAttributeGroup(group: XAttributeGroup)(implicit tag: HostTag): IndexedSeq[Tagged[_]] =
    toThat(group, tag).toIndexedSeq ++
    processAttrSeq(group.xAttrDeclsSequence3)

  def processTopLevelElement(elem: XTopLevelElement)(implicit tag: HostTag, schema: XSchema): IndexedSeq[Tagged[_]] =
    toThat(elem, tag).toIndexedSeq ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => processLocalComplexType(x, tag / "_")
      case x: XLocalSimpleType  => processLocalSimpleType(x, tag / "_")
    }} getOrElse Vector())

  def processTopLevelAttribute(attr: XTopLevelAttribute)(implicit tag: HostTag): IndexedSeq[Tagged[_]] =
    toThat(attr, tag).toIndexedSeq ++
    (attr.simpleType map { x => processLocalSimpleType(x, tag / "_") } getOrElse Vector())

  def processLocalAttribute(attr: XAttribute, childtag: HostTag): IndexedSeq[Tagged[_]] =
    toThat(attr, childtag).toIndexedSeq ++
    (attr.simpleType map { x => processLocalSimpleType(x, childtag / "_") } getOrElse Vector())

  def processAnyAttribute(anyAttribute: XWildcardable)(implicit tag: HostTag): Vector[TaggedAttr[_]] =
    Vector(TaggedAnyAttribute(anyAttribute, tag))

  def processAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): IndexedSeq[Tagged[_]] =
    (Vector(attrSeq.xattrdeclsoption1: _*).zipWithIndex flatMap {
      case (DataRecord(_, _, x: XAttribute), i)         => processLocalAttribute(x, tag / i.toString)
      case (DataRecord(_, _, x: XAttributeGroupRef), i) => processAttributeGroup(x)
    }) ++
    (attrSeq.anyAttribute map {processAnyAttribute} getOrElse Vector())
}

case class ComplexTypeOps(decl: TaggedType[XComplexType]) {
  def effectiveMixed: Boolean =
    ComplexTypeIteration.complexTypeEffectiveMixed(decl)
  def hasSimpleContent: Boolean =
    ComplexTypeIteration.complexTypeHasSimpleContent(decl)
  def simpleContentRoot(implicit lookup: Lookup): TaggedType[_] =
    ComplexTypeIteration.complexTypeToSimpleContentRoot(decl)
  def base(implicit lookup: Lookup): TaggedType[_] =
    ComplexTypeIteration.complexTypeToBaseType(decl)
  def particles(implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[_]] =
    ComplexTypeIteration.complexTypeToParticles(decl)
  def splitNonEmptyParticles(implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToSplitNonEmptyParticles(decl)
  def primaryCompositor(implicit lookup: Lookup): Option[TaggedParticle[_]] =
    ComplexTypeIteration.primaryCompositor(decl)
  def primarySequence(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
    ComplexTypeIteration.primarySequence(decl)
  def primaryChoice(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
    ComplexTypeIteration.primaryChoice(decl)
  def primaryAll(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
    ComplexTypeIteration.primaryAll(decl)

  def compositors(implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToCompositors(decl)

  def flattenedCompositors(implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToFlattenedCompositors(decl)

  def flattenedGroups(implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToFlattenedGroups(decl)

  def flattenedAttributes(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): Vector[TaggedAttr[_]] =
    ComplexTypeIteration.complexTypeToMergedAttributes(decl)

  def attributeGroups(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeIteration.complexTypeToAttributeGroups(decl)

  def descendants(implicit lookup: Lookup): Vector[TaggedType[XComplexType]] =
    lookup.ComplexType.descendants(decl)
}

class ComplexTypeIteration(underlying: IndexedSeq[Tagged[_]]) extends scala.collection.IndexedSeqLike[Tagged[_], ComplexTypeIteration] {
  lazy val length: Int = seq.length
  def apply(index: Int): Tagged[_] = seq(index)

  override def isEmpty = underlying.isEmpty
  override def toIndexedSeq = underlying.toIndexedSeq
  override def toSeq = underlying.toSeq
  override def seq = underlying.seq
  override def iterator = underlying.iterator
  override protected[this] def newBuilder: Builder[Tagged[_], ComplexTypeIteration] = ComplexTypeIteration.newBuilder
}

object ComplexTypeIteration {
  import Defs._

  def apply(decl: Tagged[XComplexType])(implicit schema: XSchema): ComplexTypeIteration =
    new ComplexTypeIteration(complexTypeToSeq(decl))
  def fromSeq(seq: IndexedSeq[Tagged[_]]): ComplexTypeIteration = new ComplexTypeIteration(seq)

  def newBuilder: Builder[Tagged[_], ComplexTypeIteration] =
    new ArrayBuffer[Tagged[_]] mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[ComplexTypeIteration, Tagged[_], ComplexTypeIteration] =
    new CanBuildFrom[ComplexTypeIteration, Tagged[_], ComplexTypeIteration] {
      def apply(): Builder[Tagged[_], ComplexTypeIteration] = newBuilder
      def apply(from: ComplexTypeIteration): Builder[Tagged[_], ComplexTypeIteration] = newBuilder
    }

  def complexTypeEffectiveMixed(decl: Tagged[XComplexType]): Boolean =
    decl.xComplexTypeModelOption3.value match {
      case x: XSimpleContent => false
      case _ => decl.mixed
    }

  /** returns sequence of tagged objects from the given complex type. */
  def complexTypeToSeq(decl: Tagged[XComplexType])(implicit schema: XSchema): IndexedSeq[Tagged[_]] = {
    implicit val tag = decl.tag
    val childtag = tag / "_"

    // <xs:group ref="xs:typeDefParticle"/>
    // <xs:group ref="xs:simpleRestrictionModel"/>
    def processRestriction(restriction: XRestrictionTypable): IndexedSeq[Tagged[_]] =
      (restriction.xrestrictiontypableoption map { _ match {
        case DataRecord(_, _, XSimpleRestrictionModelSequence(Some(simpleType), _)) =>
          SchemaIteration.processLocalSimpleType(simpleType, childtag)
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
        case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroup(KeyedGroup(key, x, childtag), childtag)
        case _ => Vector()
      }} getOrElse Vector()) ++ SchemaIteration.processAttrSeq(restriction.xAttrDeclsSequence4)

    def processExtension(extension: XExtensionTypable): IndexedSeq[Tagged[_]] =
      (extension.xTypeDefParticleOption3 map {
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
        case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroup(KeyedGroup(key, x, childtag), childtag)
        case _ => Vector()
      } getOrElse Vector()) ++ SchemaIteration.processAttrSeq(extension.xAttrDeclsSequence4)

    IndexedSeq(decl) ++
    (decl.value.xComplexTypeModelOption3.value match {
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
        } getOrElse Vector()) ++ SchemaIteration.processAttrSeq(arg2)
    })
  }

  def complexTypeToSplitNonEmptyParticles(decl: TaggedType[XComplexType])
    (implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[_]] =
    decl.primaryCompositor map {
      case x: TaggedGroupRef =>
        if (lookup.isEmptyCompositor(x)) Vector()
        else Vector(x)
      case x: TaggedKeyedGroup if x.value.key == ChoiceTag =>
        if (lookup.isEmptyCompositor(x)) Vector()
        else Vector(x)
      case x: TaggedKeyedGroup if x.value.key == AllTag => Vector(x)
      case tagged: TaggedKeyedGroup if tagged.value.key == SequenceTag =>
        implicit val tag = tagged.tag
        val split = splitter.splitLongSequence(tagged) getOrElse {decl.particles}
        split filter { x => !lookup.isEmptyCompositor(x) }
    } getOrElse Vector()

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
  def complexTypeToParticles(decl: TaggedType[XComplexType])
    (implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[_]] = {
    import lookup.{schema, ComplexType}
    implicit val tag = decl.tag
    val childtag = tag / "_"
    implicit val unbound = schema.unbound

    def processRestriction(restriction: XRestrictionTypable): IndexedSeq[TaggedParticle[_]] = {
      val base: QualifiedName = restriction.base
      base match {
        // TaggedTypes are not TaggedParticle
        // case BuiltInType(tagged) => IndexedSeq(tagged)
        // case SimpleType(tagged)  => IndexedSeq(tagged)

        // if base is a complex type, keep the same for inheritance, otherwise it should be anyType
        case ComplexType(tagged) => complexTypeToParticles(tagged)

        // restriction of anyType
        case _ => restriction.xrestrictiontypableoption map { _ match {
          // see http://www.w3.org/TR/xmlschema-1/#Complex_Type_Definitions for details.
          //case DataRecord(_, _, x@XSimpleRestrictionModelSequence(_, _)) =>
          //  x.simpleType map { simpleType => IndexedSeq(Tagged(simpleType, tag)) } getOrElse Vector()

          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, _, x: XGroupRef) => processGroupRef(x, childtag)
          case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
          case _ => Vector()
        }} getOrElse Vector()
      } // base match
    } // processRestriction

    def processExtension(extension: XExtensionTypable): IndexedSeq[TaggedParticle[_]] =  {
      val base: QualifiedName = extension.base
      base match {
        // TaggedTypes are not TaggedParticle
        // case BuiltInType(tagged) => IndexedSeq(tagged)
        // case SimpleType(tagged)  => IndexedSeq(tagged)
        case ComplexType(tagged) =>
          extension.xTypeDefParticleOption3 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, _, x: XGroupRef)          =>
              complexTypeToParticles(tagged) ++ processGroupRef(x, childtag)
            case DataRecord(_, Some(key), x: XExplicitGroupable) =>
              complexTypeToParticles(tagged) ++ processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
            case _ => complexTypeToParticles(tagged)
          } getOrElse { complexTypeToParticles(tagged) }

        // extension of anyType.
        case _ =>
          extension.xTypeDefParticleOption3 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, _, x: XGroupRef)                  => processGroupRef(x, childtag)
            case DataRecord(_, Some(key), x: XExplicitGroupable) => processKeyedGroupParticle(KeyedGroup(key, x, childtag), childtag)
            case _ => Vector()
          } getOrElse Vector()
      } // base match
    } // processExtension

    decl.value.xComplexTypeModelOption3.value match {
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
          case _ => Vector()
        } getOrElse Vector()
    }
  }

  private[scalaxb] def complexTypeHasSimpleContent(decl: TaggedType[XComplexType]): Boolean =
    decl.value.xComplexTypeModelOption3.value match {
      case x: XSimpleContent => true
      case _ => false
    }

  // root of simple content
  private[scalaxb] def complexTypeToSimpleContentRoot(decl: TaggedType[XComplexType])
    (implicit lookup: Lookup): TaggedType[_] = {
    decl.base match {
      case x: TaggedSymbol => x: TaggedType[_]
      case x: TaggedSimpleType => x: TaggedType[_]
      case x: TaggedComplexType => complexTypeToSimpleContentRoot(x)
      case _ => TaggedXsAnyType
    }
  }

  private[scalaxb] def complexTypeToBaseType(decl: Tagged[XComplexType])
    (implicit lookup: Lookup): TaggedType[_] = {
    import lookup.{BuiltInType, SimpleType, ComplexType}
    def processBase(base: QualifiedName): TaggedType[_] =
      base match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged)  => tagged
        case ComplexType(tagged) => tagged
        case _ => TaggedXsAnyType
      } // base match

    decl.value.xComplexTypeModelOption3.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processBase(x.base)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processBase(x.base)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processBase(x.base)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processBase(x.base)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) => TaggedXsAnyType
    }
  }

  private[scalaxb] def processGroupRef(ref: XGroupRef, childtag: HostTag): IndexedSeq[TaggedParticle[_]] =
    IndexedSeq(Tagged(ref, childtag))

  private[scalaxb] def processKeyedGroup(group: KeyedGroup, childtag: HostTag)(implicit schema: XSchema): IndexedSeq[Tagged[_]] =
    IndexedSeq(Tagged(group, childtag)) ++
    (group.particles flatMap {
      case TaggedLocalElement(x, form, tag) => processLocalElement(x, tag)
      case TaggedGroupRef(x, tag)     => processGroupRef(x, tag)
      case TaggedKeyedGroup(x, tag)   => processKeyedGroup(x, tag)
      case TaggedWildCard(x, tag)     => Vector()
    }) 

  // all, choice, and sequence are XExplicitGroupable, which are XGroup.
  // <xs:element name="element" type="xs:localElement"/>
  // <xs:element name="group" type="xs:groupRef"/>
  // <xs:element ref="xs:all"/>
  // <xs:element ref="xs:choice"/>
  // <xs:element ref="xs:sequence"/>
  // <xs:element ref="xs:any"/>
  private[scalaxb] def processNamedGroupParticle(particleKey: String, particle: XParticleOption, childtag: HostTag)
                                  (implicit schema: XSchema): IndexedSeq[Tagged[_]] =
    particle match {
      case x: XLocalElementable  => processLocalElement(x, childtag)
      case x: XGroupRef          => processGroupRef(x, childtag)
      
      case x: XAny               => Vector()
    }

  private[scalaxb] def processLocalElement(elem: XLocalElementable, childtag: HostTag)(implicit schema: XSchema): IndexedSeq[Tagged[_]] =
    IndexedSeq(Tagged(elem, schema.elementFormDefault, childtag)) ++
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => SchemaIteration.processLocalComplexType(x, childtag / "_")
      case x: XLocalSimpleType  => SchemaIteration.processLocalSimpleType(x, childtag / "_")
    }} getOrElse Vector())

  private[scalaxb] def processKeyedGroupParticle(group: KeyedGroup, childtag: HostTag)
      (implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[_]] =
    if ((group.key == SequenceTag) && Occurrence(group).isSingle) group.particles
    else IndexedSeq(Tagged(group, childtag))

  def namedGroupToPrimaryCompositor(value :XNamedGroup, tag: HostTag)(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
    value.xParticleOption3.toSeq.headOption match {
      case Some(DataRecord(_, Some(particleKey), x)) =>
        x match {
          case compositor: XExplicitGroupable =>
            implicit val unbound = lookup.schema.unbound
            Some(Tagged(KeyedGroup(particleKey, compositor, tag / 0.toString), tag / 0.toString))
          case _ => None
        }
      case _ => None
    }

  // unlike processKeyedGroup don't traverse into local element or group refrences
  def namedGroupToCompositors(value: XNamedGroup, tag: HostTag)(implicit lookup: Lookup): IndexedSeq[TaggedParticle[KeyedGroup]] = {
    def process(group: KeyedGroup, childtag: HostTag)(implicit schema: XSchema): IndexedSeq[TaggedParticle[KeyedGroup]] =
      IndexedSeq(Tagged(group, childtag)) ++
      (group.particles flatMap {
        case TaggedKeyedGroup(x, tag) => process(x, tag)
        case _ => Vector()
      })
    namedGroupToPrimaryCompositor(value, tag) match {
      case Some(primary) =>
        implicit val unbound = lookup.schema.unbound
        process(primary.value, primary.tag) collect {
          case Compositor(compositor) => compositor
        }
      case _ => Vector()
    }
  }

  // List of TaggedElement, TaggedKeyedGroup, or TaggedAny.
  def namedGroupToParticles(value :XNamedGroup, tag: HostTag)(implicit lookup: Lookup, splitter: Splitter): IndexedSeq[TaggedParticle[_]] =
    namedGroupToPrimaryCompositor(value, tag) match {
      case Some(tagged: TaggedKeyedGroup) => IndexedSeq(tagged)
      case _ => Vector()
    }

  /** returns all grouprefs involved in the given complex type,
   * including the ones from base type.
   */
  def complexTypeToFlattenedGroups(decl: Tagged[XComplexType])
        (implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[XGroupRef]] = {
    import lookup.{ComplexType, resolveNamedGroup, schema}
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, _, x: XGroupRef)) =>
        implicit val tag = decl.tag
        val childtag = tag / "_"
        val tagged = Tagged(x, childtag)
        Vector(tagged)
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        implicit val tag = decl.tag
        implicit val unbound = schema.unbound
        val childtag = tag / "_"
        val compositor = Tagged(KeyedGroup(key, x, childtag), childtag)
        compositor.particles collect {
          case x: TaggedGroupRef => x
        }
      case _ => Vector()
    }

    def qnameGroups(base: QualifiedName): IndexedSeq[TaggedParticle[XGroupRef]] = base match {
      case ComplexType(tagged) => complexTypeToFlattenedGroups(tagged)
      case _ => Vector()
    }

    decl.value.xComplexTypeModelOption3.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) =>
        qnameGroups(x.base) ++ extract(x.xrestrictiontypableoption)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          =>
        qnameGroups(x.base) ++ extract(x.xTypeDefParticleOption3)
      case XSimpleContent(_, _, _, _)                                                => Vector()
      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2)                                    => extract(arg1)
    }
  }

  def complexTypeToFlattenedCompositors(decl: Tagged[XComplexType])
      (implicit lookup: Lookup, splitter: Splitter, targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[_]] = {
    import lookup.{ComplexType, resolveNamedGroup, schema}
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, _, x: XGroupRef))          =>
        implicit val tag = decl.tag
        val childtag = tag / "_"
        val compositor = Tagged(resolveNamedGroup(x.ref.get), childtag)
        val tagged = Tagged(x, childtag)
        Vector(tagged) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        implicit val tag = decl.tag
        implicit val unbound = schema.unbound
        val childtag = tag / "_"
        val compositor = Tagged(KeyedGroup(key, x, childtag), childtag)
        Vector(compositor) ++ (compositor.particles collect  {
          case Compositor(c) => c
        })
      case _ => Vector()
    }

    def qnameCompositors(base: QualifiedName): IndexedSeq[TaggedParticle[_]] = base match {
      case ComplexType(tagged) => complexTypeToFlattenedCompositors(tagged)
      case _ => Vector()
    }

    decl.value.xComplexTypeModelOption3.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) =>
        qnameCompositors(x.base) ++ extract(x.xrestrictiontypableoption)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          =>
        qnameCompositors(x.base) ++ extract(x.xTypeDefParticleOption3)
      case XSimpleContent(_, _, _, _)                                                => Vector()
      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2)                                    => extract(arg1)
    }
  }

  def primarySequence(decl: TaggedType[XComplexType])(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
    primaryCompositor(decl) flatMap { _ match {
      case x: TaggedKeyedGroup if x.value.key == SequenceTag => Some(x)
      case _ => None
    }}

  def primaryChoice(decl: TaggedType[XComplexType])(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
      primaryCompositor(decl) flatMap { _ match {
        case x: TaggedKeyedGroup if x.value.key == ChoiceTag => Some(x)
        case _ => None
      }}

  def primaryAll(decl: TaggedType[XComplexType])(implicit lookup: Lookup): Option[TaggedParticle[KeyedGroup]] =
      primaryCompositor(decl) flatMap { _ match {
        case x: TaggedKeyedGroup if x.value.key == AllTag => Some(x)
        case _ => None
      }}

  def primaryCompositor(decl: TaggedType[XComplexType])(implicit lookup: Lookup): Option[TaggedParticle[_]] = {
    implicit val unbound = lookup.schema.unbound
    
    def extract(model: Option[DataRecord[Any]]) = model match {
      // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
      case Some(DataRecord(_, Some(key), x: XGroupRef))          =>
        Some(Tagged(x, decl.tag / "_"))
      case Some(DataRecord(_, Some(key), x: XExplicitGroupable)) =>
        Some(Tagged(KeyedGroup(key, x, decl.tag / "_"), decl.tag / "_"))
      case _ => 
        decl.base match {
          case base: TaggedComplexType => base.primaryCompositor
          case _ => None
        }
    }

    decl.value.xComplexTypeModelOption3.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) =>
        extract(x.xrestrictiontypableoption)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          =>
        extract(x.xTypeDefParticleOption3)

      case XSimpleContent(_, _, _, _)                                                => None
      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2)                                    => extract(arg1)
    }
  }

  def complexTypeToCompositors(decl: TaggedType[XComplexType])
                      (implicit lookup: Lookup, splitter: Splitter,
                       targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[TaggedParticle[KeyedGroup]] = {
    implicit val unbound = lookup.schema.unbound
    val ps = primarySequence(decl)

    // the primary sequence could appear as particles if it has multiple occurrence
    ps.toIndexedSeq ++
    (decl.particles collect {
      case Compositor(compositor) if Some(compositor) != ps => compositor
    })
  }

  def complexTypeToAttributeGroups(decl: TaggedType[XComplexType])
                      (implicit lookup: Lookup,
                       targetNamespace: Option[URI], scope: NamespaceBinding): IndexedSeq[Tagged[_]] = {
    import lookup.resolveAttributeGroup
    implicit val s = lookup.schema.unbound
    (decl collect  {
      case x: TaggedAttributeGroup => x.ref map { resolveAttributeGroup(_) } getOrElse x
    }).toIndexedSeq
  }

  /** attributes of the given decl flattened one level.
   * returns list of Tagged[XAttributable], Tagged[XAttributeGroup], Tagged[XWildCardable].
   */
  def complexTypeToMergedAttributes(decl: TaggedType[XComplexType])
                      (implicit lookup: Lookup,
                       targetNamespace: Option[URI], scope: NamespaceBinding): Vector[TaggedAttr[_]] = {
    import lookup.{ComplexType, resolveAttribute, resolveAttributeGroup}
    implicit val tag = decl.tag

    def qnameAttributes(base: QualifiedName) = base match {
      case ComplexType(tagged) => complexTypeToMergedAttributes(tagged)
      case _ => Vector()
    }
    
    def isSameAttribute(lhs: TaggedAttr[_], rhs: TaggedAttr[_]): Boolean = {
      (lhs, rhs) match {
        case (l: TaggedAnyAttribute, r: TaggedAnyAttribute) => true
        case (l: TaggedTopLevelAttribute, r: TaggedTopLevelAttribute) =>
          QualifiedName(decl.tag.namespace, l.value.name, l.value.ref) ==
          QualifiedName(decl.tag.namespace, r.value.name, r.value.ref)
        case (l: TaggedLocalAttribute, r: TaggedLocalAttribute) =>
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
    def mergeAttributeSeqs(base: Vector[TaggedAttr[_]], children: Vector[TaggedAttr[_]]): Vector[TaggedAttr[_]] = {
      def mergeAttribute(base: Vector[TaggedAttr[_]], child: TaggedAttr[_]): Vector[TaggedAttr[_]] =
        if (base exists { x => isSameAttribute(x, child) }) base
        else base :+ child

      children.headOption map { x =>
        mergeAttributeSeqs(mergeAttribute(base, x), children.tail)
      } getOrElse base
    }

    def processRestriction(restriction: XRestrictionTypable) =
      mergeAttributeSeqs(qnameAttributes(restriction.base),
        flattenAttrSeq(restriction.xAttrDeclsSequence4))

    def processExtension(extension: XExtensionTypable) =
      mergeAttributeSeqs(qnameAttributes(extension.base),
        flattenAttrSeq(extension.xAttrDeclsSequence4))

    // move anyAttribute to the end.
    def reorderAttributes(xs: Vector[TaggedAttr[_]]): Vector[TaggedAttr[_]] = {
      val (l, r) = xs partition {
        case x: TaggedAnyAttribute => true
        case _ => false
      }
      r ++ l
    }

    // Resolve references as walking through the attributes.
    def flattenAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): Vector[TaggedAttr[_]] =
      (Vector(attrSeq.xattrdeclsoption1: _*) flatMap {
        case DataRecord(_, _, x: XAttribute)      =>
          x.ref map { ref => Vector(resolveAttribute(ref))
          } getOrElse { Vector(Tagged(x, tag)) }
        case DataRecord(_, _, x: XAttributeGroupRef) =>
          x.ref map { ref => 
            val group = resolveAttributeGroup(ref)
            group.flattenedAttributes
          } getOrElse { flattenAttrSeq(x.xAttrDeclsSequence3) }
      }) ++
      ((attrSeq.anyAttribute map {SchemaIteration.processAnyAttribute} getOrElse Vector()): Vector[TaggedAttr[_]])

    val retval = decl.value.xComplexTypeModelOption3.value match {
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
    import lookup.Element
    tagged.value.ref match {
      case Some(Element(x)) => x
      case _ => tagged
    }
  }

  def localType(implicit lookup: Lookup): Option[TaggedType[_]] = {
    import lookup.{resolveType}
    val elem = resolve.value
    val typeValue = elem.typeValue map { resolveType(_) }
    typeValue match {
      case Some(x) => None
      case None => getLocalType
    }
  }

  private[this] def getLocalType(implicit lookup: Lookup): Option[TaggedType[_]] = {
    val elem = resolve.value
    elem.xelementoption map { _ match {
      case DataRecord(_, _, x: XLocalSimpleType)  => Tagged(x, tagged.tag / "_")
      case DataRecord(_, _, x: XLocalComplexType) => Tagged(x, tagged.tag / "_")
    }}
  }

  def typeStructure(implicit lookup: Lookup): TaggedType[_] = {
    import lookup.{resolveType}
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

  def isSubstitutionGroup(implicit lookup: Lookup): Boolean = resolve match {
    case x: TaggedTopLevelElement       => substitutionGroupMembers.size > 0
    case TaggedLocalElement(elem, _, _) =>
      elem.ref map { _ =>
        substitutionGroupMembers.size > 0
      } getOrElse false
  }

  def substitutionGroupMembers(implicit lookup: Lookup) =
    lookup.Element.substitutionGroupMembers(resolve)

  def toLocal: TaggedLocalElement = tagged match {
    case elem: TaggedLocalElement => elem
    case TaggedTopLevelElement(elem, tag) =>
      TaggedLocalElement(XLocalElement(
        ref = Some(QualifiedName(tag.namespace, elem.name.getOrElse("")).qname),
        minOccurs = 1,
        maxOccurs = "1",
        nillable = false,
        abstractValue = false,
        attributes = Map()), XQualified, tag)
  }
}

class AttributeGroupOps(val tagged: TaggedAttr[XAttributeGroup]) {
  import Defs._
  def flattenedAttributes(implicit lookup: Lookup): Vector[TaggedAttr[_]] =
    (SchemaIteration.processAttrSeq(tagged.value.xAttrDeclsSequence3)(tagged.tag) collect {
      case x: TaggedTopLevelAttribute => x
      case x: TaggedAnyAttribute      => x
      case x: TaggedAttributeGroup    => x.resolve
      case x: TaggedLocalAttribute    => x.resolve
    }).toVector

  def resolve(implicit lookup: Lookup): TaggedAttr[XAttributeGroup] =
    lookup.resolveAttributeGroup(tagged) 
}

case class AttributeOps(tagged: TaggedAttr[XAttributable]) {
  import Defs._
  def taggedType(implicit lookup: Lookup): TaggedType[_] = {
    val x = resolve
    (x.typeValue, x.simpleType) match {
      case (Some(ref), _) => lookup.resolveType(ref)
      case (_, Some(tpe)) => taggedSimpleType.get
      case _              => sys.error("taggedType # unsupported: " + tagged)
    }
  }

  def taggedSimpleType: Option[TaggedType[XSimpleType]] = {
    tagged.value.simpleType map { tpe =>
      Tagged(tpe, tagged.tag / "_")
    }
  }

  def resolve(implicit lookup: Lookup): TaggedAttr[XAttributable] =
    lookup.resolveAttribute(tagged)
}

