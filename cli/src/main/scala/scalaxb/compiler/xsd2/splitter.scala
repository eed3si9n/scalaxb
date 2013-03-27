package scalaxb.compiler.xsd2

import xmlschema._
import Defs._

trait Splitter { self: ContextProcessor with Lookup =>
  import scalaxb.compiler.Log
  private def logger = Log.forName("xsd2.Splitter")

  def splitIfLongSequence(tagged: TaggedParticle[KeyedGroup]): Vector[TaggedParticle[KeyedGroup]] =
    splitLongSequence(tagged) getOrElse {Vector(tagged)}

  def splitLongSequence(tagged: TaggedParticle[KeyedGroup]): Option[Vector[TaggedParticle[KeyedGroup]]] =
    tagged.value match {
      case x: KeyedGroup if x.key == SequenceTag =>
        splitParticles(tagged.particles, tagged.tag)
      case _ => None
    }
  private[this] def splitParticles(particles: Vector[TaggedParticle[_]], tag: HostTag): Option[Vector[TaggedParticle[KeyedGroup]]] = {
    def buildSequence(ps: Vector[TaggedParticle[_]], i: Int): TaggedKeyedGroup =
      TaggedKeyedGroup(KeyedGroup(
        key = SequenceTag,
        particles = ps,
        minOccurs = 1,
        maxOccurs = "1"), tag / ("split" + i.toString))

    def longList(ps: Vector[TaggedParticle[_]]): Option[Vector[TaggedParticle[_]]] =
      if (ps.size >= config.contentsSizeLimit) Some(ps)
      else None

    def splitLong(ps: Vector[TaggedParticle[_]], i: Int): Vector[TaggedKeyedGroup] =
      if (ps.size <= config.sequenceChunkSize) Vector(buildSequence(ps, i))
      else Vector(buildSequence(ps.take(config.sequenceChunkSize), i)) ++ splitLong(ps.drop(config.sequenceChunkSize), i + 1)

    longList(particles) map { xs => splitLong(xs, 0) }
  }
}
