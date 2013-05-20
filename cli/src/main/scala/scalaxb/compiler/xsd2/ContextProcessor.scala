package scalaxb.compiler.xsd2

import scalaxb.compiler.{ScalaNames, Config, ReferenceNotFound}
import scala.collection.mutable
import com.weiglewilczek.slf4s.Logger
import xmlschema._
import Defs._
import scalaxb._

trait ContextProcessor extends ScalaNames { self: Namer =>
  private lazy val logger: Logger = Logger("xsd2.ContextProcessor")
  def config: Config
  def context: SchemaContext
  lazy val names = context.names

  def processSchema(schema: ReferenceSchema) {
    logger.debug("processSchema")
    schema.unbound foreach {
      case tagged: TaggedTopLevelElement =>  nameElementTypes(tagged)
      case tagged: TaggedSimpleType =>
        tagged.value match {
          case x: XTopLevelSimpleType => nameSimpleTypes(tagged)
          case _ =>
        }
      case tagged: TaggedComplexType =>
        tagged.value match {
          case x: XTopLevelComplexType => nameComplexTypes(tagged)
          case _ =>
        }
      case _ =>
    }

    schema.unbound foreach {
      case tagged: TaggedLocalElement => nameElementTypes(tagged)
      case _ =>
    }
  }

  def containsEnumeration(tagged: Tagged[Any]): Boolean = tagged match {
    case x: TaggedSimpleType => !filterEnumeration(x).isEmpty
    case _ => false
  }

  def containsEnumeration(decl: XSimpleType)(implicit tag: HostTag): Boolean =
    !filterEnumeration(decl).isEmpty

  def filterEnumeration(tagged: Tagged[XSimpleType]): Seq[Tagged[XNoFixedFacet]] =
    filterEnumeration(tagged.value)(tagged.tag)

  def filterEnumeration(decl: XSimpleType)(implicit tag: HostTag): Seq[Tagged[XNoFixedFacet]] =
    decl.arg1.value match {
      case restriction: XRestriction =>
        restriction.arg1.arg2 collect {
          case DataRecord(_, Some("enumeration"), enum: XNoFixedFacet) => TaggedEnum(enum, tag)
        }
      case _ => Nil
    }
}
