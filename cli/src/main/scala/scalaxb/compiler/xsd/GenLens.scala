package scalaxb.compiler.xsd

import scalaxb.compiler.{Config, ScalaNames}

trait GenLens { self: ContextProcessor =>
  def buildImport: String
  def buildDefLens(className: String, param: Params#Param): String
  def buildDefComposeLens(className: String, param: Params#Param): String
  def buildObjectLens(localName: String, defLenses: String, defComposeLenses: String): String
}

/**
 * This class generates lens for Monocle. it is inspired from Gerolf Seitz work: Lensed
 *
 * object PurchaseOrderType {
 *   def shipTo: monocle.Lens[PurchaseOrderType, ipo.Addressable] = monocle.Lens[PurchaseOrderType, ipo.Addressable](_.shipTo)((shipTo: ipo.Addressable) => (purchaseordertype: PurchaseOrderType) => purchaseordertype.copy(shipTo = shipTo))
 *   def billTo: monocle.Lens[PurchaseOrderType, ipo.Addressable] = monocle.Lens[PurchaseOrderType, ipo.Addressable](_.billTo)((billTo: ipo.Addressable) => (purchaseordertype: PurchaseOrderType) => purchaseordertype.copy(billTo = billTo))
 *   def comment: monocle.Lens[PurchaseOrderType, Option[String]] = monocle.Lens[PurchaseOrderType, Option[String]](_.comment)((comment: Option[String]) => (purchaseordertype: PurchaseOrderType) => purchaseordertype.copy(comment = comment))
 *   def items: monocle.Lens[PurchaseOrderType, ipo.Items] = monocle.Lens[PurchaseOrderType, ipo.Items](_.items)((items: ipo.Items) => (purchaseordertype: PurchaseOrderType) => purchaseordertype.copy(items = items))
 *   def attributes: monocle.Lens[PurchaseOrderType, Map[String, scalaxb.DataRecord[Any]]] = monocle.Lens[PurchaseOrderType, Map[String, scalaxb.DataRecord[Any]]](_.attributes)((attributes: Map[String, scalaxb.DataRecord[Any]]) => (purchaseordertype: PurchaseOrderType) => purchaseordertype.copy(attributes = attributes))
 *
 *   class PurchaseOrderTypeW[A](l: monocle.Lens[A, PurchaseOrderType]) {
 *     def shipTo: monocle.Lens[A, ipo.Addressable] = l composeLens PurchaseOrderType.shipTo
 *     def billTo: monocle.Lens[A, ipo.Addressable] = l composeLens PurchaseOrderType.billTo
 *     def comment: monocle.Lens[A, Option[String]] = l composeLens PurchaseOrderType.comment
 *     def items: monocle.Lens[A, ipo.Items] = l composeLens PurchaseOrderType.items
 *     def attributes: monocle.Lens[A, Map[String, scalaxb.DataRecord[Any]]] = l composeLens PurchaseOrderType.attributes
 *   }
 *
 *   implicit def lens2PurchaseOrderTypeW[A](l: monocle.Lens[A, PurchaseOrderType]): PurchaseOrderTypeW[A] = new PurchaseOrderTypeW(l)
 * }
 * @param config
 */
class GenMonocleLens(var config: Config) extends GenLens with ContextProcessor {
  lazy val scalaNames: ScalaNames = new ScalaNames {}
  def escapeKeyWord(name: String) = if(scalaNames.isKeyword(name)) s"`$name`" else name

  def buildImport: String  = {
    ""
  }

  def buildDefLens(className : String, param: Params#Param) : String = {
    s"def ${param.toParamName}: monocle.Lens[$className, ${param.typeName}] = " +
    s"monocle.Lens[$className, ${param.typeName}](_.${param.toParamName})" +
    s"((_${param.toParamName}: ${param.typeName}) => (${escapeKeyWord(className.toLowerCase)}: $className) => ${escapeKeyWord(className.toLowerCase)}.copy(${param.toParamName} = _${param.toParamName}))"
  }

  def buildDefComposeLens(className : String, param: Params#Param) : String = {
    s"def ${param.toParamName}: monocle.Lens[A, ${param.typeName}] = l composeLens ${className}.${param.toParamName}"
  }

  override def buildObjectLens(localName: String, defLenses: String, defComposeLenses: String): String = {
    newline + "object " + {localName} + " {" + newline +
      indent(1) + defLenses + newline + newline +
      indent(1) + "implicit class " + {localName} + "W[A](l: monocle.Lens[A, " + {localName} + "]) {" + newline +
      indent(2) + defComposeLenses + newline + indent(1) + "}" + newline + newline +
      newline + "}" + newline
  }
}
