package scalaxb.compiler.xsd2

import scalaxb._
import xmlschema._
import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import Occurrence._
import java.net.URI

trait Params { self: Namer with Lookup =>
  import com.weiglewilczek.slf4s.Logger
  private lazy val logger: Logger = Logger("xsd2.Params")

  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    occurrence: Occurrence,
    attribute: Boolean) {

    def baseTypeName: QualifiedName = buildTypeName(typeSymbol)

    def singleTypeName: String =
      if (occurrence.nillable) "Option[%s]".format(baseTypeName.toScalaCode)
      else baseTypeName.toScalaCode

    def typeName(implicit targetNamespace: Option[URI]): String = occurrence match {
      case SingleNotNillable   => singleTypeName
      case SingleNillable      => singleTypeName
      case OptionalNotNillable => "Option[%s]".format(singleTypeName)
      case OptionalNillable    => "Option[%s]".format(singleTypeName)
      case _ => "Seq[%s]".format(singleTypeName)
    }

    def toTraitScalaCode: String =
      makeParamName(name) + ": " + typeName

    def toScalaCode(implicit targetNamespace: Option[URI]): String =
      toTraitScalaCode + (
        if (occurrence == OptionalNotNillable && attribute) " = None"
        else "")
  }

  object Param {
    def fromSeq(particles: Seq[Tagged[Any]]): Seq[Param] = {
      var anyNumber: Int = 0
      particles map { tagged => tagged.value match {
        case any: XAny =>
          anyNumber += 1
          buildParam(tagged, anyNumber)
        case _         => buildParam(tagged, 0)
      }}
    }

    // tagged can be Tagged[XSimpleType], Tagged[BuiltInSymbol], Tagged[XLocalElementable], Tagged[KeyedGroup],
    // Tagged[XAny].
    private def buildParam(tagged: Tagged[Any], postfix: Int) = tagged match {
      case TaggedSimpleType(decl, tag) => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
      case TaggedSymbol(symbol, tag)   => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
      case x: TaggedLocalElement       => buildElementParam(x)
      case x: TaggedKeyedGroup if x.key == ChoiceTag => buildChoiceParam(x)
      case x: TaggedKeyedGroup         => buildCompositorParam(x)
      case x: TaggedAny                => buildAnyParam(x, postfix)
      case x: TaggedAttributeParam     => buildAttributeParam(x)
      case _ => error("buildParam: " + tagged)
    }

    private def buildElementParam(tagged: Tagged[XLocalElementable]): Param = {
      val elem = tagged.value
      val name = elem.name.getOrElse(elem.ref map {_.toString} getOrElse {"??"})
      val typesymbol = elem.name map { _ =>
        elem.typeValue map { typeValue =>
          resolveType(typeValue)
        } getOrElse {
          elem.xelementoption map { _.value match {
            case x: XLocalComplexType => Tagged(x, tagged.tag)
            case x: XLocalSimpleType  => Tagged(x, tagged.tag)
          }} getOrElse {error("type not found for element: " + tagged.value.toString)}
        }
      } getOrElse { Tagged(XsInt, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "int")) }

      val retval = Param(tagged.tag.namespace, name, typesymbol, Occurrence(elem), false)
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
        Occurrence(choice).copy(nillable = false), false)
    }

    private def particleType(particle: Tagged[_]) = particle match {
      case elem: TaggedLocalElement => Some(elem.typeStructure)
      case _ => None
    }

    private def buildCompositorParam(tagged: Tagged[KeyedGroup]): Param = {
      val compositor = tagged.value
      val name = names.get(tagged) map {_.toLowerCase} getOrElse {"??"}
      val typeSymbol = tagged
      Param(tagged.tag.namespace, name, typeSymbol, Occurrence(compositor), false)
    }

    private def buildAnyParam(tagged: Tagged[XAny], postfix: Int): Param = {
      val any = tagged.value
      val name = if (postfix <= 1) "any"
        else "any" + postfix.toString
      val retval = Param(tagged.tag.namespace, name, tagged, Occurrence(any), false)
      logger.debug("buildAnyParam:  " + retval.toString)
      retval
    }

    private def buildAttributeParam(tagged: Tagged[AttributeParam]): Param = {
      val name = "attributes"
      val retval = Param(tagged.tag.namespace, name, tagged, SingleNotNillable, false)
      logger.debug("buildAttributeParam:  " + retval.toString)
      retval
    }
  } // object Param
}
