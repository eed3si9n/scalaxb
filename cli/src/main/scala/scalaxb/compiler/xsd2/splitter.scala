package scalaxb.compiler.xsd2

import xmlschema._
import Defs._

trait Splitter { self: ContextProcessor with Lookup =>
  import com.codahale.logula.Log
  
  private def logger = Log.forName("xsd2.Splitter")

  def splitIfLongSequence(tagged: TaggedParticle[KeyedGroup]): Seq[TaggedParticle[KeyedGroup]] =
    splitLongSequence(tagged) getOrElse {Seq(tagged)}

  def splitLongSequence(tagged: TaggedParticle[KeyedGroup]): Option[Seq[TaggedParticle[KeyedGroup]]] =
    tagged.value match {
      case x: KeyedGroup if x.key == SequenceTag =>
        splitParticles(tagged.particles, tagged.tag)
      case _ => None
    }

  // called by generator.
  def splitParticlesIfLong(particles: Seq[TaggedParticle[_]], tag: HostTag): Seq[TaggedParticle[_]] = {
    val retval = splitParticles(particles, tag) getOrElse {particles}
    logger.debug("splitParticlesIfLong: %s", retval map {getName(_)})
    retval
  }

  def splitParticles(particles: Seq[TaggedParticle[_]], tag: HostTag): Option[Seq[TaggedParticle[KeyedGroup]]] = {
    def buildSequence(ps: Seq[TaggedParticle[_]], i: Int): TaggedKeyedGroup =
      TaggedKeyedGroup(KeyedGroup(
        key = SequenceTag,
        particles = ps,
        minOccurs = 1,
        maxOccurs = "1"), tag / ("split" + i.toString))

    def longList(ps: Seq[TaggedParticle[_]]): Option[Seq[TaggedParticle[_]]] =
      if (ps.size >= config.contentsSizeLimit) Some(ps)
      else None

    def splitLong(ps: Seq[TaggedParticle[_]], i: Int): Seq[TaggedKeyedGroup] =
      if (ps.size <= config.sequenceChunkSize) Seq(buildSequence(ps, i))
      else Seq(buildSequence(ps.take(config.sequenceChunkSize), i)) ++ splitLong(ps.drop(config.sequenceChunkSize), i + 1)

    longList(particles) map { xs => splitLong(xs, 0) }
  }
}
