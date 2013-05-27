package scalaxb.compiler.xsd2

import scalashim._
import java.net.URI
import javax.xml.namespace.QName
import xmlschema._
import scalaxb._
import scalaxb.compiler.{ScalaNames, Config, Snippet, ReferenceNotFound, Log}
import scalaxb.compiler.xsd.{XsAnyType, XsAnySimpleType, XsString, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsWildcard, XsNillableAny}
import Defs._
import scala.xml.NamespaceBinding
import treehugger.forest._
import definitions._
import treehuggerDSL._

case class QualifiedName(namespace: Option[URI], localPart: String) {
  override def toString: String = namespace map { ns => "{%s}%s".format(ns.toString, localPart) } getOrElse {localPart}
  def qname: QName = new QName(namespace map {_.toString} orNull, localPart)
}

object QualifiedName {
  def apply(namespace: URI, name: String): QualifiedName = QualifiedName(Some(namespace), name)

  /** returns QualifiedName if ref is present; otherwise, try with namespace and name.  */
  def apply(namespace: Option[URI], name: Option[String], ref: Option[QName]): QualifiedName =
    ref map { apply(_) } getOrElse { QualifiedName(namespace, name.get) }

  implicit def apply(qname: QName): QualifiedName =
    QualifiedName(Option[String](qname.getNamespaceURI) map {new URI(_)}, qname.getLocalPart)
}

trait Lookup extends ContextProcessor { self: Namer with Splitter with Symbols =>
  private[this] val logger = Log.forName("xsd2.Lookup")

  implicit val lookupEv = this;
  def schema: ReferenceSchema
  implicit lazy val scopeEv: NamespaceBinding = schema.scope
  implicit lazy val targetNamespaceEv = schema.targetNamespace

  val wildCardType = DataRecordAnyClass
  val nillableWildCardType = DataRecordOptionAnyClass

  def buildNillableType(typ: Type): Type = TYPE_OPTION(typ)

  def buildBuiltinType(symbol: BuiltInSimpleTypeSymbol): Type = symbol.name match {
    case "XsAnySimpleType"      => wildCardType
    case "Int"                  => IntClass
    case "String"               => StringClass
    case "javax.xml.datatype.Duration" => DurationClass
    case "javax.xml.datatype.XMLGregorianCalendar" => XMLGregorianCalendarClass
    case "Boolean"              => BooleanClass
    case "Float"                => FloatClass
    case "scalaxb.Base64Binary" => Base64BinaryClass
    case "scalaxb.HexBinary"    => HexBinaryClass
    case "Double"               => DoubleClass
    case "java.net.URI"         => URIClass
    case "javax.xml.namespace.QName" => QNameClass
    case "Seq[String]"          => TYPE_SEQ(StringClass)
    case "BigDecimal"           => BigDecimalClass
    case "BigInt"               => BigIntClass
    case "Long"                 => LongClass
    case "Short"                => ShortClass
    case "Byte"                 => ByteClass
    case _ => StringClass
  }

  def buildBaseType(tagged: TaggedKeyedGroup): Type =
    tagged.key match {
      case ChoiceTag => userDefinedClassSymbol(tagged)
      case _ => buildType(tagged)
    }

  def buildType(tagged: Tagged[Any]): Type = tagged match {
    case x: TaggedDataRecordSymbol =>
      DataRecordClass TYPE_OF (x.value.member match {
        case AnyLike(any) => (AnyClass: Type)
        case x => buildType(x)
      })
    case x: TaggedOptionSymbol =>
      TYPE_OPTION((x.value.member match {
        case AnyLike(any) => (AnyClass: Type)
        case x => buildType(x)
      }))
    case x: TaggedWildCard => wildCardType
    case x: TaggedSymbol =>
      x.value match {
        case XsNillableAny =>  nillableWildCardType
        case XsAnySimpleType | XsAnyType => DataRecordAnyClass
        case symbol: BuiltInSimpleTypeSymbol => buildBuiltinType(symbol)
      }
    case x: TaggedSimpleType  => buildSimpleTypeType(x)   
    case x: TaggedComplexType => buildComplexTypeSymbol(x)
    case x: TaggedEnum        => buildEnumTypeSymbol(x)
    case x: TaggedGroupRef    => buildNamedGroupSymbol(resolveNamedGroup(x))
    case x: TaggedKeyedGroup =>
      x.key match {
        case AllTag    => MapStringDataRecordAnyClass
        case ChoiceTag =>
          // choice tag should return the choice trait here.
          // DataRecord wrapping is done at the Param level.
          userDefinedClassSymbol(tagged)
        case _ => userDefinedClassSymbol(tagged)
      }
    case TaggedMixedSeqParam(_, _)  => wildCardType
    case TaggedAttributeSeqParam(_, _) =>
      MapStringDataRecordAnyClass
    case x: TaggedAttribute =>
      x.value.typeValue map { ref => buildType(resolveType(ref)) } getOrElse {
        buildSimpleTypeType(Tagged(x.value.simpleType.get, x.tag)) }
    case x: TaggedAttributeGroup => buildAttributeGroupTypeSymbol(x)
    case _ => sys.error("buildTypeName # unsupported: " + tagged)
  }

  def buildAttributeGroupTypeSymbol(tagged: Tagged[XAttributeGroup]): ClassSymbol =
    tagged.value.ref map { ref => buildAttributeGroupTypeSymbol(resolveAttributeGroup(ref)) } getOrElse {
      userDefinedClassSymbol(tagged) }    

  def buildComplexTypeSymbol(tagged: TaggedType[XComplexType]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  def buildTraitSymbol(tagged: TaggedType[XComplexType]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getTraitName(tagged))

  def buildNamedGroupSymbol(tagged: Tagged[XNamedGroup]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  def buildKeyedGroupTypeSymbol(tagged: Tagged[KeyedGroup]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))
    
  def buildEnumTypeSymbol(tagged: Tagged[XNoFixedFacet]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  // build corresponding Scala type from simple type.
  // union uses String.
  // list uses Seq of base type.
  // restriction uses the bottom most enumeration type or the bottom builtin type.
  def buildSimpleTypeType(decl: TaggedType[XSimpleType]): Type = {
    def buildSymbol = userDefinedClassSymbol(decl.tag.namespace, getName(decl))

    decl.xSimpleDerivationOption3.value match {
      case list: XList => TYPE_SEQ(buildType(decl.baseType))
      // union baseType is hardcoded to xs:string.
      case union: XUnion => buildType(decl.baseType)

      case XRestriction(_, _, _, Some(base), _) if containsEnumeration(decl) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => buildSymbol
          case SimpleType(tagged) if containsEnumeration(tagged) => buildSimpleTypeType(tagged)
          case SimpleType(tagged) => buildSymbol
        }
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => buildType(tagged)
          case SimpleType(tagged)  => buildSimpleTypeType(tagged)
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) if containsEnumeration(decl) =>
        val t = Tagged(simpleType, decl.tag)
        if (containsEnumeration(t)) buildSimpleTypeType(t)
        else buildSymbol
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        buildSimpleTypeType(Tagged(simpleType, decl.tag))
      case _ => sys.error("buildSimpleTypeType#: Unsupported content " + decl.xSimpleDerivationOption3.value.toString)
    }
  }

  def userDefinedClassSymbol(tagged: Tagged[Any]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  def userDefinedClassSymbol(namespace: Option[URI], localPart: String): ClassSymbol = {
    val pkg = packageSymbol(namespace).moduleClass
    pkg.newClass(localPart)
  }

  def packageSymbol(namespace: Option[URI]): Symbol =
    RootClass.newPackage(packageName(namespace))

  private def formatterName(sym: ClassSymbol): String = {
    val pkg = sym.owner
    val lastPart = pkg.decodedName.split('.').reverse.head
    lastPart.capitalize + sym.decodedName + "Format"
  }

  def defaultFormatterSymbol(sym: ClassSymbol): ClassSymbol = {
    val pkg = sym.owner
    pkg.newClass("Default" + formatterName(sym))
  }

  def formatterSymbol(sym: ClassSymbol): ClassSymbol = {
    val pkg = sym.owner
    pkg.newClass(formatterName(sym))
  }

  def isRootEnumeration(tagged: Tagged[XSimpleType]): Boolean =
    if (!containsEnumeration(tagged)) false
    else tagged.value.xSimpleDerivationOption3.value match {
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => true
          case SimpleType(tagged) => !containsEnumeration(tagged)
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        !containsEnumeration(Tagged(simpleType, tagged.tag))
      case _ => false
    }

  // empty compositors are excluded from params.
  def isEmptyCompositor(tagged: TaggedParticle[Any])(implicit splitter: Splitter): Boolean =
    tagged match {
      case x: TaggedGroupRef           =>
        val group = resolveNamedGroup(x)
        group.primaryCompositor map {isEmptyCompositor} getOrElse {true}
      case x: TaggedKeyedGroup if x.key == AllTag => false
      case x: TaggedKeyedGroup if x.key == ChoiceTag =>
        if (x.particles.isEmpty) true
        else x.particles forall {isEmptyCompositor}
      case x: TaggedKeyedGroup if x.key == SequenceTag => x.particles.isEmpty
      case _ => false
    }  

  def resolveType(typeName: QualifiedName): TaggedType[_] = typeName match {
    case BuiltInAnyType(tagged) => tagged
    case BuiltInType(tagged) => tagged
    case SimpleType(tagged) => tagged
    case ComplexType(tagged) => tagged
    case _ => throw new ReferenceNotFound("type", typeName.namespace map { _.toString }, typeName.localPart)
  }

  def resolveElement(tagged: Tagged[XElement]): Tagged[XElement] =
    tagged.ref map { resolveElement(_) } getOrElse {tagged}

  def resolveElement(elementRef: QualifiedName): Tagged[XElement] = elementRef match {
    case Element(elem) => elem
    case _ => throw new ReferenceNotFound("element", elementRef.namespace map { _.toString }, elementRef.localPart)
  }

  def resolveAttribute(attrRef: QualifiedName): TaggedAttr[XAttributable] = attrRef match {
    case Attribute(attr) => attr
    case _ => throw new ReferenceNotFound("attribute", attrRef.namespace map { _.toString }, attrRef.localPart)
  }

  def resolveAttributeGroup(groupRef: QualifiedName): TaggedAttr[XAttributeGroup] = groupRef match {
    case AttributeGroup(group) => group
    case _ => throw new ReferenceNotFound("attributeGroup", groupRef.namespace map { _.toString }, groupRef.localPart)
  }

  def resolveNamedGroup(tagged: Tagged[XGroupRef]): Tagged[XNamedGroup] =
    resolveNamedGroup(tagged.value.ref.get)
  def resolveNamedGroup(groupRef: QualifiedName): Tagged[XNamedGroup] = groupRef match {
    case NamedGroup(group) => group
    case _ => throw new ReferenceNotFound("namedGroup", groupRef.namespace map { _.toString }, groupRef.localPart)    
  }

  def elementNamespace(tagged: Tagged[XElement]): Option[URI] = tagged.value match {
    case elem: XTopLevelElement => tagged.tag.namespace
    case _ =>
      if (tagged.qualified) tagged.tag.namespace
      else None
  }

  def elementNamespace(topLevelElement: Boolean, namespace: Option[URI], qualified: Boolean): Option[URI] =
    if (topLevelElement) namespace
    else if (qualified) namespace
    else None

  def elementNamespaceTree(tagged: Tagged[XElement]): Tree = optionUriTree(elementNamespace(tagged))
  def elementNamespaceTree(topLevelElement: Boolean, namespace: Option[URI], qualified: Boolean) =
    optionUriTree(elementNamespace(topLevelElement, namespace, qualified))

  object BuiltInAnyType {
    def unapply(qname: QName): Option[TaggedType[XsTypeSymbol]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[TaggedType[XsTypeSymbol]] = typeName match {
      case XS_ANY_TYPE => Some(TaggedXsAnyType)
      case XS_ANY_SIMPLE_TYPE => Some(TaggedXsAnyType)
      case _ => None
    }
  }

  object BuiltInType {
    def unapply(qname: QName): Option[TaggedType[XsTypeSymbol]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[TaggedType[XsTypeSymbol]] = typeName match {
      case QualifiedName(Some(XML_SCHEMA_URI), localPart) =>
        Some(Tagged(XsTypeSymbol.toTypeSymbol(localPart), HostTag(typeName.namespace, SimpleTypeHost, localPart)))
      case _ => None
    }
  }

  object SimpleType {
    def unapply(qname: QName): Option[TaggedType[XSimpleType]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[TaggedType[XSimpleType]] = typeName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(localPart) match {
          case x: TaggedSimpleType => Some(x)
          case _ => None
        }
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topTypes.get(localPart) match {
          case Some(x: TaggedSimpleType) => Some(x)
          case _ => None
        }}).headOption
    }
  }

  object ComplexType {
    def unapply(qname: QName): Option[TaggedType[XComplexType]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[TaggedType[XComplexType]] = typeName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(localPart) match {
          case x: TaggedComplexType => Some(x)
          case _ => None
        }
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topTypes.get(localPart) match {
          case Some(x: TaggedComplexType) => Some(x)
          case _ => None
        }}).headOption
    }

    def descendants(tagged: TaggedType[XComplexType]): Vector[TaggedType[XComplexType]] =
      Vector(context.baseToSubs(tagged): _*) flatMap { child =>
        child +: (
          if (context.baseToSubs contains child) descendants(child)
          else Vector()
        )
      }
  }

  object Element {
    def unapply(qname: QName): Option[Tagged[XElement]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[Tagged[XElement]] = qualifiedName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topElems contains localPart =>
        Some(schema.topElems(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topElems.get(localPart) }).headOption
    }

    def substitutionGroupMembers(tagged: Tagged[XElement]): IndexedSeq[TaggedTopLevelElement] =
      context.schemas.toIndexedSeq flatMap {
        _.topElems.valuesIterator.toIndexedSeq collect {
          case elem: TaggedTopLevelElement if (elem.name == tagged.name) && (elem.tag.namespace == tagged.tag.namespace) => elem
          case elem: TaggedTopLevelElement if (elem.substitutionGroup map {QualifiedName.apply}) == 
            Some(QualifiedName(tagged.tag.namespace, tagged.name getOrElse "")) => elem 
        }
      }
  }

  object Attribute {
    def unapply(qname: QName): Option[TaggedAttr[XAttributable]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[TaggedAttr[XAttributable]] = qualifiedName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topAttrs contains localPart =>
        Some(schema.topAttrs(localPart))
      case QualifiedName(Some(XML_URI), localPart) =>
        Some(xmlAttrs(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topAttrs.get(localPart) }).headOption
    }

    val xmlAttrs = Map[String, TaggedAttr[XAttributable]](
      ("lang" -> Tagged(XTopLevelAttribute(name = Some("lang"), typeValue = Some(XS_STRING_TYPE.qname), use = XOptional, attributes = Map()),
                   HostTag(Some(XML_URI), AttributeHost, "lang", "lang"))),
      ("space" -> Tagged(XTopLevelAttribute(name = Some("space"), typeValue = Some(XS_STRING_TYPE.qname), use = XOptional, attributes = Map()),
                   HostTag(Some(XML_URI), AttributeHost, "space", "space"))),
      ("base" -> Tagged(XTopLevelAttribute(name = Some("base"), typeValue = Some(XS_ANYURI_TYPE.qname), use = XOptional, attributes = Map()),
                   HostTag(Some(XML_URI), AttributeHost, "base", "base"))),
      ("id" -> Tagged(XTopLevelAttribute(name = Some("id"), typeValue = Some(XS_ID_TYPE.qname), use = XOptional, attributes = Map()),
                   HostTag(Some(XML_URI), AttributeHost, "id", "id")))
    )
  }

  object NamedGroup {
    def unapply(qname: QName): Option[Tagged[XNamedGroup]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[Tagged[XNamedGroup]] = qualifiedName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topGroups contains localPart =>
        Some(schema.topGroups(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topGroups.get(localPart) }).headOption
    }    
  }

  object AttributeGroup {
    def unapply(qname: QName): Option[TaggedAttr[XAttributeGroup]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[TaggedAttr[XAttributeGroup]] = qualifiedName match {
      case QualifiedName(`targetNamespaceEv`, localPart) if schema.topAttrGroups contains localPart =>
        Some(schema.topAttrGroups(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topAttrGroups.get(localPart) }).headOption
    }
  }

  object AnyLike {
    def unapply(tagged: Tagged[_]): Option[Tagged[_]] = tagged match {
      case x: TaggedWildCard => Some(tagged)
      case x: TaggedSymbol =>
        x.value match {
          case XsAnySimpleType | XsAnyType | XsNillableAny => Some(tagged)
          case symbol: BuiltInSimpleTypeSymbol => None
        }
      case _ => None
    }
  }

  def schemasByNamespace(namespace: Option[URI]): Seq[ReferenceSchema] =
    context.schemas filter {_.targetNamespace == namespace}

  def isForeignType(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedLocalElement => x.value.ref map { QualifiedName(_).namespace != targetNamespaceEv } getOrElse { false }
    case x: TaggedGroupRef =>
      x.value.ref map { QualifiedName(_).namespace != targetNamespaceEv } getOrElse { false }
    case _ => false
  }

  def isOptionDescendant(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedLocalElement =>
      x.typeStructure match {
        case decl: TaggedComplexType => true
        case _ => false
      }
    case group: TaggedKeyedGroup if group.value.key == ChoiceTag =>
      group.value.particles forall { isOptionDescendant }
    case group: TaggedKeyedGroup if group.value.key == SequenceTag => true
    case _ => false
  }

  def packageName(namespace: Option[URI]): String =
    (if (namespace == Some(SCALAXB_URI)) Some("scalaxb")
    else if (config.packageNames.contains(namespace map {_.toString})) config.packageNames(namespace map {_.toString})
    else if (config.packageNames.contains(None)) config.packageNames(None)
    else None) getOrElse {
    "generated"
  }
}
