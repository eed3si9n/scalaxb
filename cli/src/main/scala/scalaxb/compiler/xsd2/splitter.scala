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
      case KeyedGroup(SequenceTag, _) =>
        implicit val tag = tagged.tag
        splitParticles(tagged.particles)
      case _ => None
    }

  // called by generator.
  def splitParticlesIfLong(particles: Seq[TaggedParticle[_]])(implicit tag: HostTag): Seq[TaggedParticle[_]] = {
    val retval = splitParticles(particles) getOrElse {particles}
    logger.debug("splitParticlesIfLong: %s", retval map {getName(_)})
    retval
  }

  def splitParticles(particles: Seq[TaggedParticle[_]])(implicit tag: HostTag): Option[Seq[TaggedParticle[KeyedGroup]]] = {
    def buildSequence(ps: Seq[TaggedParticle[_]]): TaggedKeyedGroup =
      TaggedKeyedGroup(KeyedGroup(SequenceTag, XExplicitGroup(annotation = None,
             arg1 = ps map {Tagged.toParticleDataRecord},
             minOccurs = 1,
             maxOccurs = "1",
             attributes = Map())), tag)

    def longList(ps: Seq[TaggedParticle[_]]): Option[Seq[TaggedParticle[_]]] =
      if (ps.size >= config.contentsSizeLimit) Some(ps)
      else None

    def splitLong(ps: Seq[TaggedParticle[_]]): Seq[TaggedKeyedGroup] =
      if (ps.size <= config.sequenceChunkSize) Seq(buildSequence(ps))
      else Seq(buildSequence(ps.take(config.sequenceChunkSize))) ++ splitLong(ps.drop(config.sequenceChunkSize))

    longList(particles) map { xs => splitLong(xs) }
  }
}
