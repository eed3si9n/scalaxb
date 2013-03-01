/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler.xsd

import scalashim._
import scalaxb.compiler.Log
import scala.collection.mutable
import scala.collection.immutable

trait Parsers extends Args with Params {
  private val logger = Log.forName("xsd.Parsers")
  // called by makeCaseClassWithType and buildSeqParser
  def buildParser(particle: Particle, mixed: Boolean, wrapInDataRecord: Boolean, ignoreSubGroup: Boolean): String =
    buildParser(particle, buildOccurrence(particle), mixed, wrapInDataRecord, ignoreSubGroup)
    
  def buildParser(particle: Particle, occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean, ignoreSubGroup: Boolean): String = particle match {
    case elem: ElemDecl           => buildElemParser(elem, occurrence, mixed, wrapInDataRecord, ignoreSubGroup)
    case ref: ElemRef             => buildElemParser(buildElement(ref), occurrence, mixed, wrapInDataRecord, ignoreSubGroup)
    case ref: GroupRef            => buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)   
    case compositor: HasParticle  => buildCompositorParser(compositor, occurrence, mixed, wrapInDataRecord, ignoreSubGroup)
    case any: AnyDecl             => buildAnyParser(any.namespaceConstraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
  }
  
  def buildAnyParser(namespaceConstraint: List[String], occurrence: Occurrence, mixed: Boolean,
                     wrapInDataRecord: Boolean, laxAny: Boolean): String = {
    val converter = if (occurrence.nillable) buildFromXML("scalaxb.DataRecord[Option[Any]]", "_",
        "scalaxb.ElemName(node) :: stack", None)
      else buildFromXML(buildTypeName(XsWildcard(namespaceConstraint)), "_", "scalaxb.ElemName(node) :: stack", None)
    val parser = "any(%s)".format(
      if (laxAny) "_ => true"
      else namespaceConstraint match {
        case ("##any" :: Nil) | Nil | ("" :: Nil) => "_ => true"
        case "##other" :: Nil => "_.namespace != %s" format (quoteNamespace(schema.targetNamespace))
        case _ =>
          """x => %s contains x.namespace""" format (namespaceConstraint.map {
            case "##targetNamespace" => quoteNamespace(schema.targetNamespace)
            case "##local" => "None"
            case x => quoteNamespace(Some(x))
          }).mkString("List(", ", ", ")")
      })
    
    buildParserString(if (mixed) "((" + parser + " ^^ (" + converter + ")) ~ " + newline +
        indent(3) + buildTextParser + ") ^^ " + newline +
        indent(3) + "{ case p1 ~ p2 => Seq.concat(Seq(p1), p2.toList) }"
      else if (wrapInDataRecord) "(" + parser + " ^^ (" + converter + "))"
      else parser,
      occurrence)
  }
  
  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildCompositorParser(compositor: HasParticle, occurrence: Occurrence, 
      mixed: Boolean, wrapInDataRecord: Boolean, ignoreSubGroup: Boolean): String = compositor match {
    case ref: GroupRef        => buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)
    case seq: SequenceDecl    => 
      if (containsSingleChoice(seq)) buildChoiceParser(singleChoice(seq), occurrence, mixed, ignoreSubGroup)
      else buildSeqParser(seq, occurrence, mixed, wrapInDataRecord, ignoreSubGroup)
    case choice: ChoiceDecl   => buildChoiceParser(choice, occurrence, mixed, ignoreSubGroup)
    case all: AllDecl         => buildAllParser(all, occurrence, mixed)
    case group: GroupDecl     => buildGroupParser(group, occurrence, mixed, wrapInDataRecord)
  }
  
  def buildGroupParser(group: GroupDecl, occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): String =
    buildParserString(if (mixed) "parsemixed" + groupTypeName(group) + "(node, scalaxb.ElemName(node) :: stack)"
      else "parse" + groupTypeName(group) + (if (wrapInDataRecord) "(node, scalaxb.ElemName(node) :: stack, true)"
        else "(node, scalaxb.ElemName(node) :: stack)"),
      occurrence)
    
  // for unmixed wrapped in data record, this should generate Seq(DataRecord(None, None, Foo("1", "2")))
  // for mixed, this should generate
  // Seq(DataRecord(Some("ipo") Some("a"), "1"), DataRecord(None, None, "foo"), DataRecord(Some("ipo") Some("b"), "2"))
  def buildSeqParser(seq: SequenceDecl,
      occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean, ignoreSubGroup: Boolean): String = {
    val splits = if (mixed) seq.particles
      else if (seq.particles.size <= contentsSizeLimit) seq.particles
      else splitLong[SequenceDecl](seq.particles) { SequenceDecl(seq.namespace, _, 1, 1, 0) }

    val parserList = if (mixed) (0 to seq.particles.size * 2 - 1).toList map { i =>
        if (seq.particles.size == 0) buildTextParser
        else if (i % 2 == 0) buildParser(seq.particles(i / 2), mixed, mixed, ignoreSubGroup)
        else buildTextParser
      }
      else splits map { buildParser(_, mixed, mixed, ignoreSubGroup) }
    
    def buildSeqConverter(seq: SequenceDecl, mixed: Boolean,
        wrapInDataRecord: Boolean): String = {
      lazy val localName = makeTypeName(context.compositorNames(seq))
      lazy val fqn = buildFullyQualifiedNameFromNS(seq.namespace, localName)
      
      val particles = buildParticles(splits)
      val parserVariableList = if (mixed) (0 to particles.size * 2 - 1) map { buildSelector }
        else (0 to particles.size - 1) map { buildSelector }
      val argList = if (mixed) (0 to particles.size * 2 - 1).toList map { i =>
          if (i % 2 == 0) buildArgForMixed(particles(i / 2), i, ignoreSubGroup)
          else buildArgForOptTextRecord(i) }
        else (0 to particles.size - 1).toList map { i => buildArg(particles(i), i) }
      val paramList = if (mixed) Nil
        else particles map { buildParam }
      val hasSequenceParam = (paramList.size == 1) &&
        (paramList.head.cardinality == Multiple) &&
        (!mixed)

      def argsString = if (hasSequenceParam) argList.head + ": _*"
        else argList.mkString("," + newline + indent(4))

      "{ case " +
      (if (seq.particles.size == 0) "_"
      else  parserVariableList.mkString(" ~ ")) +
      (if (mixed) " => Seq.concat(" + argsString + ")"
      else if (wrapInDataRecord) " => scalaxb.DataRecord(" + fqn + "(" + argsString + "))"
      else " => " + fqn + "(" + argsString + ")") +
      " }"
    }
    
    val base = parserList.mkString("(", " ~ " + newline + indent(3), ")") + " ^^ " + newline +
      indent(4) + buildSeqConverter(seq, mixed, wrapInDataRecord)
    val retval = buildParserString(base, occurrence)
    logger.debug("buildSeqParser:  " + seq + newline + retval)
    retval
  }
  
  def buildAllParser(all: AllDecl, occurrence: Occurrence, mixed: Boolean): String = {
    val parserList = all.particles.map { buildParser(_, mixed, false, false) }
    
    def buildAllConverter(seq: AllDecl): String = {
      "{ case p => foo()}"
    }
    
    val base = parserList.mkString("(", " ~ " + newline + indent(3), ")") + " ^^ " + newline +
      indent(3) + buildAllConverter(all)
    buildParserString(base, occurrence)   
  }
  
  // one or more particles may be emptiable within the choices.
  // in such case, treat the whole choice to be minOccurs = 0,
  // but make sure all particles has at least minOccurs = 1.
  // additionally, treat all particles as maxOccurs = 1 and make the whole
  // choice repeatable in case any one particle is repeatable.
  // this may violate the schema, but it is a compromise as long as plurals are
  // treated as Seq[DataRecord].
  // substitution group parsing is implemented as choice parser.
  // in that case, pass ignoreSubGroup to be false.
  def buildChoiceParser(choice: ChoiceDecl, occurrence: Occurrence, mixed: Boolean, ignoreSubGroup: Boolean): String = {
    assert(choice.particles.size > 0, "choice has no particles: " + choice)
    val containsStructure = if (mixed) true
      else choice.particles exists(_ match {
        case elem: ElemDecl => false
        case ref: ElemRef => false
        case _ => true
        })
    val singleOccurrence = occurrence.copy(minOccurs = 1, maxOccurs = 1) 
    
    // expand substitution groups into elements
    var options = choice.particles flatMap { _ match {
      case any: AnyDecl => Nil
      case compositor: HasParticle if isEmptyCompositor(compositor) => Nil
      case elem: ElemDecl =>
        if (isSubstitutionGroup(elem)) substitutionGroupMembers(elem)
        else Seq(elem)
      case ref: ElemRef =>
        val elem = buildElement(ref)
        if (isSubstitutionGroup(elem)) substitutionGroupMembers(elem)
        else Seq(ref)
      case particle => Seq(particle)
    }}
    val parserList = options map {
      case elem: ElemDecl =>
        if (mixed && containsStructure) buildParser(SequenceDecl(elem.namespace, List(elem), 1, 1, 0), singleOccurrence, mixed, true, true)
        else buildParser(elem, singleOccurrence, mixed, true, true)
      case ref: ElemRef =>
        if (mixed && containsStructure) buildParser(SequenceDecl(ref.namespace, List(ref), 1, 1, 0), singleOccurrence, mixed, true, true)
        else buildParser(ref, singleOccurrence, mixed, true, true)
      case particle => buildParser(particle, singleOccurrence, mixed, true, true)
    }
    val choiceOperator = if (containsStructure) "|||" else "|"
    
    val nonany = if (parserList.size > 0)
      parserList.mkString(" " + choiceOperator + " " + newline + indent(3))
    else ""
    
    val anyList = choice.particles filter(
      _.isInstanceOf[AnyDecl]) map { particle =>
      buildParser(particle, singleOccurrence, mixed, true, true) }
    val base = if (anyList.size > 0)
      if (nonany == "") anyList(0)
      else "(" + nonany + ") | " + newline +
        indent(3) + anyList(0)
    else nonany
    
    val retval = buildParserString(base, occurrence)
    logger.debug("buildChoiceParser:  " + choice + newline + retval)
    retval
  }
    
  def buildSubstitionGroupParser(elem: ElemDecl, occurrence: Occurrence, mixed: Boolean): String = {
    logger.debug("buildSubstitionGroupParser")
    
    // these are the known members of the sub group.
    val particles = substitutionGroupMembers(elem)
    val choice = ChoiceDecl(elem.namespace,
      if (particles.size > 0) particles.toList
      else List(AnyDecl(elem.minOccurs, elem.maxOccurs, List("##any"), LaxProcess)),
      elem.minOccurs, elem.maxOccurs)
    buildChoiceParser(choice, occurrence, mixed, true)
  }
  
  def buildElemParser(elem: ElemDecl, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean,
                      ignoreSubGroup: Boolean): String = {
    def buildConverter(typeSymbol: XsTypeSymbol, occurrence: Occurrence): String = {
      val record = "scalaxb.DataRecord(x.namespace, Some(x.name), " + buildArg("x", typeSymbol) + ")"
      val nillableRecord = "scalaxb.DataRecord(x.namespace, Some(x.name), x.nilOption map {" + buildArg("_", typeSymbol) + "})"
      
      (toCardinality(occurrence), occurrence.nillable) match {
        case (Multiple, true)   => "(_.toSeq map { x => " + nillableRecord + " })"        
        case (Multiple, false)  => "(_.toSeq map { x => " + record + " })" 
        case (Optional, true)   => "(_ map { x => " + nillableRecord + " })"
        case (Optional, false)  => "(_ map { x => " + record + " })"      
        case (Single, true)     => "(x => " + nillableRecord + ")"
        case (Single, false)    => "(x => " + record + ")"
      }
    }

    def addConverter(p: String): String = if (wrapInDataRecord) "(" + p + " ^^ " + newline +
        indent(3) + buildConverter(elem.typeSymbol, occurrence) + ")"
      else p
    
    if (isSubstitutionGroup(elem) && !ignoreSubGroup) buildSubstitionGroupParser(elem, occurrence, mixed)
    else elem.typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => addConverter(buildParserString(elem, occurrence))
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  addConverter(buildParserString(elem, occurrence))
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (compositorWrapper.contains(decl)) {
          val compositor = compositorWrapper(decl)
          val o = buildOccurrence(compositor)
          buildCompositorParser(compositor, occurrence.copy(nillable = o.nillable), mixed, wrapInDataRecord, ignoreSubGroup)
        }
        else addConverter(buildParserString(elem, occurrence))
      case AnyType(XsWildcard(constraint)) => buildAnyParser(constraint, occurrence, mixed, wrapInDataRecord, config.laxAny)
      case AnyType(symbol) => buildAnyParser(Nil, occurrence, mixed, wrapInDataRecord, true)
      case XsLongAll => ""
      
      case symbol: ReferenceTypeSymbol =>
        if (symbol.decl == null)
          sys.error("Parsers#buildParser: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with null decl")
        else    
          sys.error("Parsers#buildParser: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with " + symbol.decl.toString)
      case _ => sys.error("Parsers#buildParser: " + elem.toString +
        " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
    }
  }
    
  def buildParserString(elem: ElemDecl, occurrence: Occurrence): String =
    buildParserString("scalaxb.ElemName(" +
      elementNamespaceString(elem.global, elem.namespace, elem.qualified) + ", " +
      quote(elem.name) + ")",
      occurrence)

  def buildParserString(base: String, occurrence: Occurrence) =
    if (occurrence.maxOccurs > 1) "rep(" + base + ")"
    else if (occurrence.minOccurs == 0) "opt(" + base + ")"
    else "(" + base + ")"
    
  def buildParticles(com: Option[HasParticle], name: String): List[ElemDecl] = com match {
    case Some(c) => buildParticles(c.particles)
    case None => Nil
  }

  def buildParticles(particles: List[Particle]): List[ElemDecl] =
    particles.zipWithIndex map {
      case (ref: GroupRef, i: Int)            => buildCompositorRef(ref, i)        
      case (seq: SequenceDecl, i: Int)        =>
        if (containsSingleChoice(seq)) buildCompositorRef(singleChoice(seq), i)
        else buildCompositorRef(seq, i)
      case (compositor2: HasParticle, i: Int) => buildCompositorRef(compositor2, i)
      case (elem: ElemDecl, i: Int)           => elem
      case (ref: ElemRef, i: Int)             => buildElement(ref)
      case (any: AnyDecl, i: Int)             => buildAnyRef(any)
    }
  
  def buildTextParser = "optTextRecord"
  
  def buildDependentType(particle: Particle): Seq[XsTypeSymbol] = particle match {
    case group: GroupDecl   => buildDependentType(primaryCompositor(group))
    case choice: ChoiceDecl => choice.particles flatMap { buildDependentType }
    case any: AnyDecl       => Nil
    case elem: ElemDecl     =>
      elem.typeSymbol match {
        case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
          if (compositorWrapper.contains(decl)) buildDependentType(compositorWrapper(decl))
          else Seq(elem.typeSymbol)
        case _ => Seq(elem.typeSymbol)
      }
    
    case ref: ElemRef       => Seq(buildElement(ref).typeSymbol)
    case ref: GroupRef      => buildDependentType(buildGroup(ref)) 
    case compositor: HasParticle => Seq(buildCompositorRef(compositor, 0).typeSymbol)
    case _                  => Nil  
  }
  
  def buildImplicitParams(dependents: List[Decl]) =
    immutable.ListMap[Decl, Param]((for (i <- 0 to dependents.size - 1)
      yield (dependents(i), 
        Param(None, "ev" + i, XsXMLFormat(dependents(i)), Single, false, false, false, false) )): _*)
  
  /// for recursive complex types, which would call this#toXML or this#fromXML.
  def buildFakeImplicitParam(decl: ComplexTypeDecl): Param =
    Param(None, "this", XsXMLFormat(decl), Single, false, false, false, false)
}
