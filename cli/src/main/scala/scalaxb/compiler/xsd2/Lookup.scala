package scalaxb.compiler.xsd2

import scalashim._
import java.net.URI
import javax.xml.namespace.QName
import xmlschema._
import scalaxb._
import scalaxb.compiler.{ScalaNames, Config, Snippet, ReferenceNotFound}
import scalaxb.compiler.xsd.{XsAnyType, XsAnySimpleType, XsString, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsWildcard}
import Defs._
import scala.xml.NamespaceBinding
import treehugger.forest._
import definitions._
import treehuggerDSL._

case class QualifiedName(namespace: Option[URI], localPart: String, parameters: QualifiedName*) {
  override def toString: String = namespace map { ns => "{%s}%s".format(ns.toString, localPart) } getOrElse {localPart}
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
  import com.codahale.logula.Log
  private[this] val logger = Log.forName("xsd2.Lookup")

  implicit val lookup = this;
  def schema: ReferenceSchema
  implicit lazy val scope: NamespaceBinding = schema.scope
  implicit lazy val targetNamespace = schema.targetNamespace

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
    case _ => StringClass
  }

  def buildType(tagged: Tagged[Any]): Type = tagged match {
    case x: TaggedDataRecordSymbol => DataRecordClass TYPE_OF buildType(x.value.member)
    case x: TaggedWildCard => wildCardType
    case x: TaggedSymbol =>
      x.value match {
        case XsAnySimpleType | XsAnyType => DataRecordAnyClass
        case symbol: BuiltInSimpleTypeSymbol => buildBuiltinType(symbol)
          //QualifiedName(Some(XML_SCHEMA_URI), symbol.name)
      }
    case x: TaggedSimpleType  => buildSimpleTypeType(x)   
    case x: TaggedComplexType => buildComplexTypeSymbol(x)
    case x: TaggedEnum        => buildEnumTypeSymbol(x)
    case x: TaggedKeyedGroup =>
      x.key match {
        case ChoiceTag =>
          val particleOs = x.value.value.arg1.toList map { Occurrence(_) }
          if (particleOs exists { _.nillable }) buildNillableType(userDefinedClassSymbol(tagged))
          else userDefinedClassSymbol(tagged)
        case _ => userDefinedClassSymbol(tagged)
      }
    case TaggedAttributeSeqParam(_, _) | TaggedAllParam(_, _) =>
      MapStringDataRecordAnyClass
    case x: TaggedAttribute =>
      x.value.typeValue map { ref => buildType(resolveType(ref)) } getOrElse {
        buildSimpleTypeType(Tagged(x.value.simpleType.get, x.tag)) }
    case x: TaggedAttributeGroup => buildAttributeGroupTypeSymbol(x)
    
    //    case XsNillableAny  => nillableAnyTypeName
    //    case XsAnyAttribute  => "Map[String, scalaxb.DataRecord[Any]]"
    //    case XsDataRecord(ReferenceTypeSymbol(decl: ComplexTypeDecl)) if compositorWrapper.contains(decl) =>
    //      compositorWrapper(decl) match {
    //        case choice: ChoiceDecl => buildChoiceTypeName(decl, choice, shortLocal)
    //        case _ => "scalaxb.DataRecord[Any]"
    //      }
    //    case r: XsDataRecord => "scalaxb.DataRecord[Any]"
    //    case XsMixed         => "scalaxb.DataRecord[Any]"
    //    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl, shortLocal)
    //    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl, shortLocal)
    //    case XsXMLFormat(decl: ComplexTypeDecl) => "scalaxb.XMLFormat[" + buildTypeName(decl, shortLocal) + "]"
    //    case XsXMLFormat(group: AttributeGroupDecl) => "scalaxb.XMLFormat[" + buildTypeName(group, shortLocal) + "]"
    case _ => sys.error("buildTypeName # unsupported: " + tagged)
  }

  def buildAttributeGroupTypeSymbol(tagged: Tagged[XAttributeGroup]): ClassSymbol =
    tagged.value.ref map { ref => buildAttributeGroupTypeSymbol(resolveAttributeGroup(ref)) } getOrElse {
      userDefinedClassSymbol(tagged) }    

  def buildComplexTypeSymbol(tagged: Tagged[XComplexType]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  def buildKeyedGroupTypeSymbol(tagged: Tagged[KeyedGroup]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))
    
  def buildEnumTypeSymbol(tagged: Tagged[XNoFixedFacet]): ClassSymbol =
    userDefinedClassSymbol(tagged.tag.namespace, getName(tagged))

  def buildSimpleTypeType(decl: Tagged[XSimpleType]): Type = {
    decl.arg1.value match {
      case restriction: XRestriction if containsEnumeration(decl) =>
        // trace type hierarchy to the top most type that implements enumeration.
        val base = baseType(decl)
        userDefinedClassSymbol(base.tag.namespace, getName(base))
      case restriction: XRestriction =>
        buildType(baseType(decl))
      case list: XList =>
        val base = baseType(decl)
        val baseName = base.value match {
          case symbol: BuiltInSimpleTypeSymbol => symbol.name
          case decl: XSimpleType => getName(base)
        }
        TYPE_SEQ(userDefinedClassSymbol(base.tag.namespace, baseName))
      // union baseType is hardcoded to xs:string.
      case union: XUnion =>
        buildType(baseType(decl))
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

  def formatterSymbol(sym: ClassSymbol): ClassSymbol = {
    val pkg = sym.owner
    val lastPart = pkg.decodedName.split('.').reverse.head
    pkg.newClass(lastPart.capitalize + sym.decodedName + "Format")
  }

  def isRootEnumeration(tagged: Tagged[XSimpleType]): Boolean =
    if (!containsEnumeration(tagged)) false
    else tagged.value.arg1.value match {
      case XRestriction(_, _, _, Some(base), _) =>
        QualifiedName(base) match {
          case BuiltInType(tagged) => true
          case SimpleType(tagged) => !containsEnumeration(tagged)
        }
      case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
        !containsEnumeration(Tagged(simpleType, tagged.tag))
      case _ => false
    }

  def baseType(decl: Tagged[XSimpleType]): Tagged[Any] = decl.value.arg1.value match {
    case XRestriction(_, _, _, Some(base), _) if containsEnumeration(decl) =>
      QualifiedName(base) match {
        case BuiltInType(tagged) => decl
        case SimpleType(tagged) =>
          if (containsEnumeration(tagged)) baseType(tagged)
          else decl
      }
    case XRestriction(_, _, _, Some(base), _) =>
      QualifiedName(base) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged) => baseType(tagged)
      }
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) if containsEnumeration(decl) =>
      if (containsEnumeration(Tagged(simpleType, decl.tag))) baseType(Tagged(simpleType, decl.tag))
      else decl
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case XList(_, _, _, Some(itemType), _) =>
      QualifiedName(itemType) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged) => baseType(tagged)
      }
    case XList(_, Some(simpleType), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case x: XUnion => Tagged(XsString, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "string"))
    case _ => sys.error("baseType#: Unsupported content " + decl.arg1.value.toString)
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

  def resolveAttribute(attrRef: QualifiedName): Tagged[XAttributable] = attrRef match {
    case Attribute(attr) => attr
    case _ => throw new ReferenceNotFound("attribute", attrRef.namespace map { _.toString }, attrRef.localPart)
  }

  def resolveAttributeGroup(groupRef: QualifiedName): TaggedAttr[XAttributeGroup] = groupRef match {
    case AttributeGroup(group) => group
    case _ => throw new ReferenceNotFound("attributeGroup", groupRef.namespace map { _.toString }, groupRef.localPart)
  }

  def resolveNamedGroup(tagged: TaggedGroupRef): Tagged[XNamedGroup] =
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
      case QualifiedName(`targetNamespace`, localPart) if schema.topTypes contains localPart =>
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
      case QualifiedName(`targetNamespace`, localPart) if schema.topTypes contains localPart =>
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
  }

  object Element {
    def unapply(qname: QName): Option[Tagged[XElement]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[Tagged[XElement]] = qualifiedName match {
      case QualifiedName(`targetNamespace`, localPart) if schema.topElems contains localPart =>
        Some(schema.topElems(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topElems.get(localPart) }).headOption
    }
  }

  object Attribute {
    def unapply(qname: QName): Option[TaggedAttr[XAttributable]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[TaggedAttr[XAttributable]] = qualifiedName match {
      case QualifiedName(`targetNamespace`, localPart) if schema.topAttrs contains localPart =>
        Some(schema.topAttrs(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topAttrs.get(localPart) }).headOption
    }
  }

  object NamedGroup {
    def unapply(qname: QName): Option[Tagged[XNamedGroup]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[Tagged[XNamedGroup]] = qualifiedName match {
      case QualifiedName(`targetNamespace`, localPart) if schema.topGroups contains localPart =>
        Some(schema.topGroups(localPart))
      case QualifiedName(ns, localPart) =>
        (schemasByNamespace(ns) flatMap { _.topGroups.get(localPart) }).headOption
    }    
  }

  object AttributeGroup {
    def unapply(qname: QName): Option[TaggedAttr[XAttributeGroup]] = unapply(qname: QualifiedName)

    def unapply(qualifiedName: QualifiedName): Option[TaggedAttr[XAttributeGroup]] = qualifiedName match {
      case QualifiedName(`targetNamespace`, localPart) if schema.topAttrGroups contains localPart =>
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
          case XsAnySimpleType | XsAnyType => Some(tagged)
          case symbol: BuiltInSimpleTypeSymbol => None
        }
      case _ => None
    }
  }

  def schemasByNamespace(namespace: Option[URI]): Seq[ReferenceSchema] =
    context.schemas filter {_.targetNamespace == namespace}

  def isForeignType(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedLocalElement => x.value.ref map { QualifiedName(_).namespace != targetNamespace } getOrElse { false }
    case x: TaggedGroupRef =>
      x.value.ref map { QualifiedName(_).namespace != targetNamespace } getOrElse { false }
    case _ => false
  }

  def isOptionDescendant(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedLocalElement =>
      x.typeStructure match {
        case decl: TaggedComplexType => true
        case _ => false
      }
    case group@TaggedKeyedGroup(KeyedGroup(ChoiceTag, _), _) =>
      group.particles forall { isOptionDescendant }
    case TaggedKeyedGroup(KeyedGroup(SequenceTag, _), _) => true
    case _ => false
  }

  def packageName(namespace: Option[URI]): String =
    namespace map { ns =>
      if (ns == SCALAXB_URI) "scalaxb"
      else "example"
    } getOrElse { "example" }
}
