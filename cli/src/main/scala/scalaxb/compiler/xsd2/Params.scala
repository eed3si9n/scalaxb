package scalaxb.compiler.xsd2

import scalaxb._
import xmlschema._
import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import Occurrence._
import java.net.URI

trait Params { self: Namer with Lookup =>
  import Predef.{any2stringadd => _}
  import com.weiglewilczek.slf4s.Logger
  private lazy val logger: Logger = Logger("xsd2.Params")

  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    occurrence: Occurrence,
    attribute: Boolean,
    topLevelElement: Boolean,
    qualified: Boolean) {

    def baseTypeName: QualifiedName = buildTypeName(typeSymbol)

    def singleTypeName: String =
      if (occurrence.nillable) "Option[%s]".format(baseTypeName.toScalaCode)
      else baseTypeName.toScalaCode

    def typeName(implicit targetNamespace: Option[URI]): String = occurrence match {
      case SingleNotNillable(_)   => singleTypeName
      case SingleNillable(_)      => singleTypeName
      case OptionalNotNillable(_) => "Option[%s]".format(singleTypeName)
      case OptionalNillable(_)    => "Option[%s]".format(singleTypeName)
      case _ => "Seq[%s]".format(singleTypeName)
    }

    def paramName: String = makeParamName(name)

    def toTraitScalaCode: String =
      paramName + ": " + typeName

    def toScalaCode(implicit targetNamespace: Option[URI]): String =
      toTraitScalaCode + (
        if (occurrence == OptionalNotNillable && attribute) " = None"
        else "")

    def toDataRecordMapAccessor(wrapper: String, generateImpl: Boolean): String =
      generateImpl match {
        case true =>
          "lazy val " + toTraitScalaCode + " = " +
          (occurrence match {
            case SingleNotNillable(_) =>
              """%s(%s).as[%s]""".format(
                wrapper,
                quote(buildNodeName),
                singleTypeName
              )
            case _ =>
              """%s.get(%s) map {_.as[%s]}""".format(
                wrapper,
                quote(buildNodeName),
                singleTypeName
              )
          })
        case _ => "def " + toTraitScalaCode
      }

    def toLongSeqAccessor(wrapper: String): String =
      """lazy val %s = %s.%s""" format(toTraitScalaCode, wrapper, makeParamName(name))

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
      case x: TaggedKeyedGroup if x.key == ChoiceTag => buildChoiceParam(x)
      case x: TaggedKeyedGroup         => buildCompositorParam(x)
      case x: TaggedWildCard           => buildWildCardParam(x, postfix)
      case x: TaggedAttributeSeqParam  => buildDataRecordMapParam(ATTRS_PARAM, x)
      case x: TaggedAllParam           => buildDataRecordMapParam(ALL_PARAM, x)
      case _ => error("buildParam: " + tagged)
    }

    private def buildElementParam(tagged: Tagged[XLocalElementable]): Param = {
      val elem = tagged.resolve
      val name = elem.name
      val retval = Param(tagged.tag.namespace, name.get, elem.typeStructure,
        Occurrence(tagged.value), false,
        elem match {
          case TaggedTopLevelElement(_, _) => true
          case _ => false
        }, elem.qualified)
      logger.debug("buildElementParam:  " + retval.toString)
      retval
    }

    private def buildChoiceParam(tagged: Tagged[KeyedGroup]): Param = {
      implicit val tag = tagged.tag
      val choice = tagged.value
      val name = names.get(tagged) map {_.toLowerCase} getOrElse {"??"}
      val particles = choice.particles

      val memberType = choice.particles match {
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
      val typeSymbol = TaggedDataRecordSymbol(DataRecordSymbol(memberType))
      Param(tagged.tag.namespace, name, typeSymbol,
        Occurrence(choice).copy(nillable = false), false, false, false)
    }

    private def particleType(particle: Tagged[_]) = particle match {
      case elem: TaggedLocalElement => Some(elem.typeStructure)
      case _ => None
    }

    private def buildCompositorParam(tagged: Tagged[KeyedGroup]): Param = {
      val compositor = tagged.value
      val name = names.get(tagged) map {_.toLowerCase} getOrElse {"??"}
      val typeSymbol = tagged
      Param(tagged.tag.namespace, name, typeSymbol, Occurrence(compositor), false, false, false)
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
