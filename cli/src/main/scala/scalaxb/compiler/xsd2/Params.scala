package scalaxb.compiler.xsd2

import scalaxb._
import xmlschema._
import scalaxb.compiler.xsd.{XsAnyType, XsNillableAny, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import Occurrence._
import java.net.URI

trait Params { self: Namer with Lookup =>
  import Predef.{any2stringadd => _}
  import com.codahale.logula.Log
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private val logger = Log.forName("xsd2.Params")

  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    occurrence: Occurrence,
    attribute: Boolean,
    topLevelElement: Boolean,
    qualified: Boolean) {

    def baseType: Type = buildType(typeSymbol)

    def singleType: Type =
      if (occurrence.nillable) buildNillableType(baseType)
      else baseType

    def typ: Type = occurrence match {
      case SingleNotNillable(_)   => singleType
      case SingleNillable(_)      => singleType
      case OptionalNotNillable(_) => TYPE_OPTION(singleType)
      case OptionalNillable(_)    => TYPE_OPTION(singleType)
      case _ => TYPE_SEQ(singleType)
    }

    def paramName: String = makeParamName(name)

    def toTraitScalaCode(implicit targetNamespace: Option[URI], lookup: Lookup): String =
      paramName + ": " + typ

    def tree(implicit targetNamespace: Option[URI], lookup: Lookup): ValDef =
      PARAM(paramName, typ)

    def varargTree(implicit targetNamespace: Option[URI], lookup: Lookup): ValDef =
      PARAM(paramName, TYPE_*(singleType))

    def toScalaCode(implicit targetNamespace: Option[URI], lookup: Lookup): String =
      toTraitScalaCode + (
        if (occurrence == OptionalNotNillable && attribute) " = None"
        else "")

    def toDataRecordMapAccessor(wrapper: String, generateImpl: Boolean)
                               (implicit targetNamespace: Option[URI], lookup: Lookup): Tree =
      generateImpl match {
        case true =>
          LAZYVAL(toTraitScalaCode) := (occurrence match {
            case SingleNotNillable(_) =>
              REF(wrapper) APPLY(LIT(buildNodeName)) DOT "as" APPLYTYPE singleType
            case _ =>
              (REF(wrapper) DOT "get" APPLY(LIT(buildNodeName))) MAP (
                WILDCARD DOT "as" APPLYTYPE singleType
              )
          })
        case _ => DEF(toTraitScalaCode)
      }

    def toLongSeqAccessor(wrapper: String): Tree =
      LAZYVAL(toTraitScalaCode) := REF(wrapper) DOT makeParamName(name)
      // """lazy val %s = %s.%s""" format(toTraitScalaCode, wrapper, makeParamName(name))

    def buildNodeName: String = typeSymbol match {
      case TaggedAttribute(x: XTopLevelAttribute, _) => "@" + QualifiedName(namespace, name).toString
      case TaggedAttribute(_, _) => "@" + QualifiedName(None, name).toString
      case _ => QualifiedName(None, name).toString
    }
  }

  object Param {
    def apply(tagged: Tagged[Any]): Param = buildParam(tagged, 0)

    def fromSeq(particles: Seq[Tagged[Any]]): Seq[Param] = {
      var anyNumber: Int = 0
      particles map { tagged => tagged.value match {
        case any: XAny =>
          anyNumber += 1
          buildParam(tagged, anyNumber)
        case _         => buildParam(tagged, 0)
      }}
    }

    // called by generateAccessors
    def fromAttributes(attributes: Seq[Tagged[_]]): Seq[Param] = attributes collect {
      case x: TaggedAttribute => buildAttributeParam(x)
    }

    // tagged can be Tagged[XSimpleType], Tagged[BuiltInSymbol], Tagged[XLocalElementable], Tagged[KeyedGroup],
    // Tagged[XAny].
    private def buildParam(tagged: Tagged[Any], postfix: Int) = tagged match {
      case TaggedSimpleType(decl, tag) => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable(), false, false, false)
      case TaggedSymbol(symbol, tag)   => Param(tagged.tag.namespace, "value", tagged, SingleNotNillable(), false, false, false)
      case x: TaggedLocalElement       => buildElementParam(x)
      case x: TaggedGroupRef           => buildGroupRefParam(x)
      case x: TaggedKeyedGroup if x.key == AllTag => buildDataRecordMapParam(ALL_PARAM, x)
      case x: TaggedKeyedGroup if x.key == ChoiceTag => buildChoiceParam(x)
      case x: TaggedKeyedGroup         => buildCompositorParam(x)
      case x: TaggedWildCard           => buildWildCardParam(x, postfix)
      case x: TaggedAttributeSeqParam  => buildDataRecordMapParam(ATTRS_PARAM, x)
      case _ => error("buildParam: " + tagged)
    }

    private def buildElementParam(tagged: Tagged[XLocalElementable]): Param = {
      val elem = tagged.resolve
      val name = elem.name
      val (typeSymbol, o) = (elem.typeStructure, Occurrence(tagged.value)) match {
        case (AnyLike(symbol), o) if o.nillable => (TaggedXsNillableAny, o.copy(nillable = false))
        case (symbol, o) => (symbol, o) 
      }
      val retval = Param(tagged.tag.namespace, name.get, typeSymbol, o, false,
        elem match {
          case TaggedTopLevelElement(_, _) => true
          case _ => false
        }, elem.qualified)
      logger.debug("buildElementParam:  " + retval.toString)
      retval
    }

    private def buildChoiceParam(tagged: TaggedParticle[KeyedGroup]): Param = {
      val name = getName(tagged).toLowerCase
      val particles = tagged.particles
      val o = Occurrence(tagged)

      val memberType = tagged.particles match {
        case Nil       => TaggedXsAnyType
        case x :: xs =>
          val sameType = x match {
            case elem: TaggedLocalElement =>
              val firstType = particleType(x)
              if (xs forall { particleType(_) == firstType }) firstType
              else None
            case _ => None
          }

          sameType getOrElse {
            if ( (particles forall { !isForeignType(_) }) &&
              (particles forall { isOptionDescendant }) ) tagged
            else TaggedXsAnyType
          }
      }
      val typeSymbol =
        if (o.nillable) TaggedDataRecordSymbol(DataRecordSymbol(TaggedOptionSymbol(OptionSymbol(memberType))))
        else TaggedDataRecordSymbol(DataRecordSymbol(memberType))
      Param(tagged.tag.namespace, name, typeSymbol,
        o.copy(nillable = false), false, false, false)
    }

    private def particleType(particle: Tagged[_]) = particle match {
      case elem: TaggedLocalElement => Some(elem.typeStructure)
      case _ => None
    }

    private def buildCompositorParam(tagged: Tagged[KeyedGroup]): Param = {
      val compositor = tagged.value
      val name = getName(tagged).toLowerCase
      val typeSymbol = tagged
      val retval = Param(tagged.tag.namespace, name, typeSymbol, Occurrence(compositor), false, false, false)
      logger.debug("buildCompositorParam: " + retval.toString)
      retval
    }

    private def buildGroupRefParam(tagged: Tagged[XGroupRef]): Param = {
      val group = resolveNamedGroup(tagged)
      val name = getName(group).toLowerCase
      val retval = Param(group.tag.namespace, name, tagged, Occurrence(tagged), false, false, false)
      logger.debug("buildGroupRefParam: " + retval.toString)
      retval.copy(occurrence = Occurrence(tagged.value))
    }

    private def buildWildCardParam(tagged: Tagged[XAny], postfix: Int): Param = {
      val any = tagged.value
      val name = if (postfix <= 1) "any"
        else "any" + postfix.toString
      val retval = Param(tagged.tag.namespace, name, tagged, Occurrence(any), false, false, false)
      logger.debug("buildWildCardParam:  " + retval.toString)
      retval
    }

    private def buildDataRecordMapParam(name: String, tagged: Tagged[_]): Param = {
      val retval = Param(tagged.tag.namespace, name, tagged, SingleNotNillable(), false, false, false)
      logger.debug("buildDataRecordMapParam:  " + retval.toString)
      retval
    }

    private def buildAttributeParam(tagged: TaggedAttr[XAttributable]): Param = {
      val attr = tagged.value
      val name = attr.name.get
      val retval = Param(tagged.tag.namespace, name, tagged, Occurrence(attr), true, false, false)
      logger.debug("buildAttributeParam:  " + retval.toString)
      retval
    }
  } // object Param
}
