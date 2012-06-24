package scalaxb.compiler.xsd2

import scalaxb.DataRecord
import xmlschema._
import Defs._
import Predef.{any2stringadd => _, _}

case class Occurrence(minOccurs: Int, maxOccurs: Int, nillable: Boolean) {
  def isMultiple = maxOccurs > 1
  def isSingle = minOccurs == 1 && maxOccurs <= 1
  def isOptional = minOccurs < 1 && maxOccurs <= 1
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

  def apply(particle: TaggedParticle[_]): Occurrence = particle match {
    case TaggedLocalElement(x, form, tag) => Occurrence(x)
    case TaggedGroupRef(x, tag)     => Occurrence(x)
    case TaggedKeyedGroup(x, tag)     => Occurrence(x)
    case TaggedWildCard(x, tag)     => Occurrence(x)
  }

  def apply(ref: XGroupRef): Occurrence =
    Occurrence(ref.minOccurs.toInt, ref.maxOccurs, false)

  def apply(keyed: KeyedGroup): Occurrence = keyed.key match {
    case ChoiceTag =>
      val o = Occurrence(keyed.minOccurs.toInt, keyed.maxOccurs, false)
      val particleOs = keyed.particles.toList map {Occurrence(_)}
      Occurrence((o.minOccurs :: (particleOs map { _.minOccurs})).min,
        (o.maxOccurs :: (particleOs map { _.maxOccurs})).max,
        particleOs exists {_.nillable})
    case SequenceTag =>
      val minValue = if (keyed.particles.isEmpty) 0
                     else keyed.minOccurs.toInt
      Occurrence(minValue, keyed.maxOccurs, false)
    case _ =>
      Occurrence(keyed.minOccurs.toInt, keyed.maxOccurs, false)
  }

  def apply(attr: XAttributable): Occurrence = attr.use match {
    case XRequired => SingleNotNillable()
    case _ => OptionalNotNillable()
  }

  def max(lhs: String, rhs: String): String =
    if (lhs == "unbounded" || rhs == "unbounded") "unbounded"
    else math.max(lhs.toInt, rhs.toInt).toString

  object Single {
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isSingle) Some(o) else None
  }

  object SingleNotNillable {
    def apply() = Occurrence(1, 1, false)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isSingle && !o.nillable) Some(o) else None
  }

  object SingleNillable {
    def apply() = Occurrence(1, 1, true)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isSingle && o.nillable) Some(o) else None
  }

  object Optional {
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isOptional) Some(o) else None
  }

  object OptionalNotNillable {
    def apply() = Occurrence(0, 1, false)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isOptional && !o.nillable) Some(o) else None
  }

  object OptionalNillable {
    def apply() = Occurrence(0, 1, true)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isOptional && o.nillable) Some(o) else None
  }

  object Multiple {
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isMultiple) Some(o) else None
  }

  object UnboundedNotNillable {
    def apply() = Occurrence(0, Int.MaxValue, false)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isMultiple && !o.nillable) Some(o) else None
  }
  
  object UnboundedNillable {
    def apply() = Occurrence(0, Int.MaxValue, true)
    def unapply(o: Occurrence): Option[Occurrence] = if (o.isMultiple && o.nillable) Some(o) else None
  }
}




