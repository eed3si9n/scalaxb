package scalaxb.compiler.xsd2

import xmlschema._
import Defs._

trait Splitter { self: ContextProcessor with Lookup =>
  def splitIfLongSequence(tagged: Tagged[KeyedGroup]): Seq[Tagged[KeyedGroup]] =
    splitLongSequence(tagged) getOrElse {Seq(tagged)}

  def splitLongSequence(tagged: Tagged[KeyedGroup]): Option[Seq[Tagged[KeyedGroup]]] =
    tagged.key match {
      case SequenceTag =>
        implicit val tag = tagged.tag
        splitParticles(tagged.particles)
      case _ => None
    }

  // called by generator.
  def splitParticlesIfLong(particles: Seq[Tagged[_]])(implicit tag: HostTag): Seq[Tagged[_]] =
    splitParticles(particles) getOrElse {particles}

  def splitParticles(particles: Seq[Tagged[_]])(implicit tag: HostTag): Option[Seq[TaggedKeyedGroup]] = {
    def buildSequence(ps: Seq[Tagged[_]]): TaggedKeyedGroup =
      TaggedKeyedGroup(KeyedGroup("sequence", XExplicitGroup(annotation = None,
             arg1 = ps map {Tagged.toParticleDataRecord},
             minOccurs = 1,
             maxOccurs = "1",
             attributes = Map())), tag)

    def longList(ps: Seq[Tagged[_]]): Option[Seq[Tagged[_]]] =
      if (ps.size >= config.contentsSizeLimit) Some(ps)
      else None

    def splitLong(ps: Seq[Tagged[_]]): Seq[TaggedKeyedGroup] =
      if (ps.size <= config.sequenceChunkSize) Seq(buildSequence(ps))
      else Seq(buildSequence(ps.take(config.sequenceChunkSize))) ++ splitLong(ps.drop(config.sequenceChunkSize))

    longList(particles) map { xs => splitLong(xs) }
  }
}
