package scalaxb.compiler.xsd2

import java.net.URI
import javax.xml.namespace.QName
import xmlschema._
import scalaxb._
import scalaxb.compiler.{ScalaNames, Config, Snippet, ReferenceNotFound}
import scalaxb.compiler.xsd.{XsAnyType, XsAnySimpleType, XsString, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsWildcard}
import Defs._
import scala.xml.NamespaceBinding
import treehugger._
import definitions._

case class QualifiedName(namespace: Option[URI], localPart: String, parameters: QualifiedName*) {
  override def toString: String = namespace map { ns => "{%s}%s".format(ns.toString, localPart) } getOrElse {localPart}
  
//  def toScalaCode(implicit targetNamespace: Option[URI], lookup: Lookup): String =
//    if (namespace == targetNamespace || namespace.isEmpty ||
//      Seq(Some(XML_SCHEMA_URI), Some(SCALA_URI)).contains(namespace)) localPart
//    else fullyQualifiedName

  def localNameType(implicit targetNamepsace: Option[URI], lookup: Lookup): Type =
    RootClass.newClass(localName.toTypeName).toType
  
  def localName(implicit targetNamespace: Option[URI], lookup: Lookup): String =
    if (namespace == targetNamespace)
      if (parameters.isEmpty) localPart
      else "%s[%s]" format (localPart, parameters.toSeq map {_.localName} mkString(", "))
    else fullyQualifiedName

  def fullyQualifiedName(implicit lookup: Lookup): String = {
    val s = namespace match {
      case Some(XML_SCHEMA_URI) | Some(SCALA_URI) => localPart
      case _ => lookup.packageName(namespace) + "." + localPart
    }

    if (parameters.isEmpty) s
    else "%s[%s]" format (s, parameters.toSeq map {_.fullyQualifiedName} mkString(", "))
  }

  def formatterName(implicit lookup: Lookup): String = {
    val pkg = lookup.packageName(namespace)
    val lastPart = pkg.split('.').reverse.head
    lastPart.capitalize + localPart + "Format"
  }

  def option: QualifiedName = QualifiedName(Some(SCALA_URI), "Option", this)
  def nillable: QualifiedName = option
  def dataRecord: QualifiedName = QualifiedName(Some(SCALAXB_URI), "DataRecord", this)
  def seq: QualifiedName = QualifiedName(Some(SCALA_URI), "Seq", this)
  def map(valueType: QualifiedName) = QualifiedName(Some(SCALA_URI), "Map", this, valueType)
}

object QualifiedName {
  def apply(namespace: URI, name: String): QualifiedName = QualifiedName(Some(namespace), name)

  /** returns QualifiedName if ref is present; otherwise, try with namespace and name.  */
  def apply(namespace: Option[URI], name: Option[String], ref: Option[QName]): QualifiedName =
    ref map { apply(_) } getOrElse { QualifiedName(namespace, name.get) }

  implicit def apply(qname: QName): QualifiedName =
    QualifiedName(Option[String](qname.getNamespaceURI) map {new URI(_)}, qname.getLocalPart)

  val AnyTypeName = QualifiedName(Some(SCALA_URI), "Any")
  val StringTypeName = QualifiedName(Some(SCALA_URI), "String")
  val DataRecordAnyTypeName = AnyTypeName.dataRecord
  val DataRecordOptionAnyTypeName = AnyTypeName.option.dataRecord
  val MapStringDataRecordAnyTypeName = StringTypeName.map(DataRecordAnyTypeName)
}

trait Lookup extends ContextProcessor { self: Namer with Splitter =>
  import com.weiglewilczek.slf4s.{ Logger }
  private lazy val logger = Logger("xsd2.Lookup")

  implicit val lookup = this;
  def schema: ReferenceSchema
  implicit lazy val scope: NamespaceBinding = schema.scope
  implicit lazy val targetNamespace = schema.targetNamespace

  val wildCardTypeName = QualifiedName.DataRecordAnyTypeName

  def buildTypeName(tagged: Tagged[Any]): QualifiedName = tagged match {
    case x: TaggedDataRecordSymbol => buildTypeName(x.value.member).dataRecord
    case x: TaggedWildCard => wildCardTypeName
    case x: TaggedSymbol =>
      x.value match {
        case XsAnySimpleType | XsAnyType => QualifiedName.DataRecordAnyTypeName
        case symbol: BuiltInSimpleTypeSymbol => QualifiedName(Some(XML_SCHEMA_URI), symbol.name)
      }
    case x: TaggedSimpleType => buildSimpleTypeTypeName(x)   
    case TaggedComplexType(_, _) | TaggedEnum(_, _) =>
      QualifiedName(tagged.tag.namespace, names.get(tagged) getOrElse {
        error("unnamed %s" format tagged.toString)
      })
    case x: TaggedKeyedGroup =>
      x.key match {
        case ChoiceTag =>
          val particleOs = x.group.arg1.toList map { Occurrence(_) }
          if (particleOs exists { _.nillable }) QualifiedName(tagged.tag.namespace, names.get(x) getOrElse { "??" }).nillable
          else QualifiedName(tagged.tag.namespace, names.get(x) getOrElse { "??" })
        case _ => QualifiedName(tagged.tag.namespace, names.get(x) getOrElse { "??" })
      }
    case TaggedAttributeSeqParam(_, _) | TaggedAllParam(_, _) =>
      QualifiedName.MapStringDataRecordAnyTypeName
    case x: TaggedAttribute =>
      x.value.typeValue map { ref => buildTypeName(resolveType(ref)) } getOrElse {
        buildSimpleTypeTypeName(Tagged(x.value.simpleType.get, x.tag)) }
    case x: TaggedAttributeGroup =>
      x.value.ref map { ref => buildTypeName(resolveAttributeGroup(ref)) } getOrElse {
        QualifiedName(tagged.tag.namespace, names.get(tagged) getOrElse {
          error("unnamed %s" format tagged.toString)
        })}
      
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
    case _ => error("buildTypeName # unsupported: " + tagged)
  }

  def buildSimpleTypeTypeName(decl: Tagged[XSimpleType]): QualifiedName = {
    decl.arg1.value match {
      case restriction: XRestriction if containsEnumeration(decl) =>
        // trace type hierarchy to the top most type that implements enumeration.
        val base = baseType(decl)
        QualifiedName(base.tag.namespace, names.get(base) getOrElse { "??" })
      case restriction: XRestriction =>
        buildTypeName(baseType(decl))
      case list: XList =>
        val base = baseType(decl)
        val baseName = base.value match {
          case symbol: BuiltInSimpleTypeSymbol => symbol.name
          case decl: XSimpleType => names.get(base) getOrElse { "??" }
        }
        QualifiedName(base.tag.namespace, baseName).seq
      // union baseType is hardcoded to xs:string.
      case union: XUnion =>
        buildTypeName(baseType(decl))
    }
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
    case _ => error("baseType#: Unsupported content " + decl.arg1.value.toString)
  }

  def resolveType(typeName: QualifiedName): Tagged[Any] = typeName match {
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

  def elementNamespaceString(tagged: Tagged[XElement]): String = quoteUri(elementNamespace(tagged))
  def elementNamespaceString(topLevelElement: Boolean, namespace: Option[URI], qualified: Boolean) =
    quoteUri(elementNamespace(topLevelElement, namespace, qualified))

  object BuiltInAnyType {
    // called by ElementOps
    val tagged = Tagged(XsAnyType, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, XS_ANY_TYPE.localPart))

    def unapply(qname: QName): Option[Tagged[XsTypeSymbol]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case XS_ANY_TYPE => Some(tagged)
      case XS_ANY_SIMPLE_TYPE => Some(tagged)
      case _ => None
    }
  }

  object BuiltInType {
    def unapply(qname: QName): Option[Tagged[XsTypeSymbol]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case QualifiedName(Some(XML_SCHEMA_URI), localPart) =>
        Some(Tagged(XsTypeSymbol.toTypeSymbol(localPart), HostTag(typeName.namespace, SimpleTypeHost, localPart)))
      case _ => None
    }
  }

  object SimpleType {
    def unapply(qname: QName): Option[Tagged[XSimpleType]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[Tagged[XSimpleType]] = typeName match {
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
    def unapply(qname: QName): Option[Tagged[XComplexType]] = unapply(qname: QualifiedName)

    def unapply(typeName: QualifiedName): Option[Tagged[XComplexType]] = typeName match {
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
    case x: TaggedKeyedGroup if x.value.key == GroupTag =>
      x.value.group.ref map { QualifiedName(_).namespace != targetNamespace } getOrElse { false }
    case _ => false
  }

  def isOptionDescendant(tagged: Tagged[_]): Boolean = tagged match {
    case x: TaggedLocalElement =>
      x.typeStructure match {
        case decl: TaggedComplexType => true
        case _ => false
      }
    case x: TaggedKeyedGroup if x.value.key == ChoiceTag =>
      implicit val tag = x.tag
      x.value.particles forall { isOptionDescendant }
    case x: TaggedKeyedGroup if x.value.key == SequenceTag => true
    case _ => false
  }

  def packageName(namespace: Option[URI]): String =
    namespace map { ns =>
      if (ns == SCALAXB_URI) "scalaxb"
      else "example"
    } getOrElse { "example" }
}
