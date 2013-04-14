package scalaxb.compiler.xsd2

import scalaxb.compiler.{ScalaNames, Config, ReferenceNotFound, Log}
import scala.collection.mutable
import xmlschema._
import Defs._
import scalaxb._

trait ContextProcessor extends ScalaNames { self: Namer with Lookup =>
  private val logger = Log.forName("xsd2.ContextProcessor")
  def config: Config
  def context: SchemaContext

  def getName(tagged: Tagged[_]): String =
    context.names.get(tagged) getOrElse {
      // error(tagged.tag.toString + "??")
      tagged.tag.toString + "??"
    }

  def getTraitName(tagged: Tagged[XComplexType]): String =
    context.traitNames.get(tagged) getOrElse {
      error(tagged.tag.toString + "??")
    }    

  def processContext() {
    logger.debug("processContext")
    context.schemas map { schema =>
      logger.debug("processContext - " + schema.unbound.toSeq)

      schema.unbound foreach {
        case tagged: TaggedComplexType =>
          tagged.base match {
            case base: TaggedComplexType => associateSubType(tagged, base)
            case _ =>
          }
        case _ =>
      }

      context.baseToSubs.keysIterator.toList foreach { base =>
        if (true
          // && context.schemas.exists(schema => context.duplicatedTypes.contains((schema, base)))
        ) {
          nameTrait(base)
        } // if
      }
    }
  }

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
      case tagged: TaggedNamedGroup => nameNamedGroup(tagged)
      case tagged: TaggedAttributeGroup =>
        tagged.value match {
          case x: XNamedAttributeGroup => nameAttributeGroup(tagged)
          case _ =>
        }
      case _ =>
    }

    schema.unbound foreach {
      case tagged: TaggedLocalElement => nameElementTypes(tagged)
      case _ =>
    }
  }

  def associateSubType(tagged: TaggedComplexType, base: TaggedComplexType) {
    logger.debug("associateSubType - " + tagged + " " + base)
    if (!context.baseToSubs.contains(base)) { context.baseToSubs(base) = Nil }
    context.baseToSubs(base) = tagged :: context.baseToSubs(base)
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
    decl.xSimpleDerivationOption3.value match {
      case restriction: XRestriction =>
        restriction.xSimpleRestrictionModelSequence3.xFacetsOption2 collect {
          case DataRecord(_, Some("enumeration"), enum: XNoFixedFacet) => TaggedEnum(enum, tag)
        }
      case _ => Nil
    }
}
