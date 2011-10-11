package scalaxb.compiler.xsd2

import scalaxb.DataRecord
import xmlschema._
import Defs._

case class Occurrence(minOccurs: Int, maxOccurs: Int, nillable: Boolean) {
  def isMultiple = maxOccurs > 1
  def isSingle = minOccurs == 1 && maxOccurs == 1
}

object Occurrence {
  def apply(minOccurs: Int, maxOccurs: String, nillable: Boolean): Occurrence =
    Occurrence(minOccurs,
      if (maxOccurs == "unbounded") Int.MaxValue
      else maxOccurs.toInt,
      nillable)

  def apply(elem: XElement): Occurrence =
    Occurrence(elem.minOccurs.toInt, elem.maxOccurs, elem.nillable)

  def apply(any: XAny): Occurrence =
    Occurrence(any.minOccurs.toInt, any.maxOccurs, false)

  def apply(particle: DataRecord[XParticleOption]): Occurrence = particle match {
    case DataRecord(_, _, x: XElement)               => Occurrence(x)
    case DataRecord(_, _, x: XAny)                   => Occurrence(x)
    case DataRecord(_, Some("group"), x: XGroup)     => Occurrence(KeyedGroup("group", x))
    case DataRecord(_, Some("all"), x: XGroup)       => Occurrence(KeyedGroup("all", x))
    case DataRecord(_, Some("choice"), x: XGroup)    => Occurrence(KeyedGroup("choice", x))
    case DataRecord(_, Some("sequence"), x: XGroup)  => Occurrence(KeyedGroup("sequence", x))
  }

  def apply(keyed: KeyedGroup): Occurrence = keyed.key match {
    case GroupTag =>
      // TODO: fix this
      Occurrence(keyed.group.minOccurs.toInt, keyed.group.maxOccurs, false)
    case ChoiceTag =>
      val choice = keyed.group
      val o = Occurrence(choice.minOccurs.toInt, choice.maxOccurs, false)
      val particleOs = choice.arg1.toList map {Occurrence(_)}
      Occurrence((o.minOccurs :: (particleOs map { _.minOccurs})).min,
        (o.maxOccurs :: (particleOs map { _.maxOccurs})).max,
        particleOs exists {_.nillable})
    case _ =>
      Occurrence(keyed.group.minOccurs.toInt, keyed.group.maxOccurs, false)
  }

  def apply(attr: XAttributable): Occurrence = attr.use match {
    case XRequired => SingleNotNillable
    case _ => OptionalNotNillable
  }

  val SingleNotNillable = Occurrence(1, 1, false)
  val SingleNillable = Occurrence(1, 1, true)
  val OptionalNotNillable = Occurrence(0, 1, false)
  val OptionalNillable = Occurrence(0, 1, true)
  val UnboundedNotNillable = Occurrence(0, Int.MaxValue, false)
  val UnboundedNillable = Occurrence(0, Int.MaxValue, true)

  def max(lhs: String, rhs: String): String =
    if (lhs == "unbounded" || rhs == "unbounded") "unbounded"
    else math.max(lhs.toInt, rhs.toInt).toString
}

