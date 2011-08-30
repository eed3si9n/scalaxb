package scalaxb.compiler.xsd

sealed trait NamespaceKind
case object SchemaKind extends NamespaceKind
case object XsdTypeKind extends NamespaceKind
case object GroupKind extends NamespaceKind
case object AttributeGroupKind extends NamespaceKind

case class NameKey(kind: NamespaceKind, namespace: Option[String], name: String)

object NameKey {
  implicit def toNameKey(schema: SchemaDecl): NameKey =
    NameKey(SchemaKind, schema.targetNamespace, schema.hashCode().toString)
  implicit def toNameKey(decl: SimpleTypeDecl): NameKey =
    NameKey(XsdTypeKind, decl.namespace, decl.name)
  implicit def toNameKey(decl: ComplexTypeDecl): NameKey =
    NameKey(XsdTypeKind, decl.namespace, decl.name)
  implicit def toNameKey(group: GroupDecl): NameKey =
    NameKey(GroupKind, group.namespace, group.name)
  implicit def toNameKey(group: AttributeGroupDecl): NameKey =
    NameKey(AttributeGroupKind, group.namespace, group.name)
  implicit def toNameKey(decl: Decl): NameKey = decl match {
    case x: SchemaDecl => toNameKey(x)
    case x: SimpleTypeDecl => toNameKey(x)
    case x: ComplexTypeDecl => toNameKey(x)
    case x: GroupDecl => toNameKey(x)
    case x: AttributeGroupDecl => toNameKey(x)
    case _ => error("unexpected Decl: " + decl.toString)
  }
}

//class NameRepository {
//  import scala.collection.mutable
//
//  val names: mutable.ListMap[NameKey, String] = mutable.ListMap()
//  def update(key: NameKey, name: String) { names(key) = name }
//  def contains(key: NameKey): Boolean = names.contains(key)
//  def getOrElseUpdate(key: NameKey, value: => String): String = names.getOrElseUpdate(key, value)
//  def apply(key: NameKey): String = names(key)
//  def containsValue(namespace: Option[String], value: String) =
//    names exists {
//      case (k: NameKey, v: String) => k.namespace == namespace && v == value
//    }
//}
