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
import scala.collection.mutable
import scala.collection.immutable

trait Parsers extends Args with Params {
  // called by makeCaseClassWithType and buildSeqParser
  def buildParser(particle: Particle, mixed: Boolean, wrapInDataRecord: Boolean): String =
    buildParser(particle, buildOccurrence(particle), mixed, wrapInDataRecord)
    
  def buildParser(particle: Particle, occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): String = particle match {
    case elem: ElemDecl       =>
      buildElemParser(elem, occurrence, mixed, wrapInDataRecord)
    case ref: ElemRef         =>
      buildElemParser(buildElement(ref), occurrence, mixed, wrapInDataRecord)
    case ref: GroupRef        =>
      buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)   
    case compositor: HasParticle =>
      buildCompositorParser(compositor, occurrence, mixed, wrapInDataRecord)
    case any: AnyDecl =>
      buildAnyParser(any, occurrence, mixed, wrapInDataRecord)
  }
  
  def buildAnyParser(any: AnyDecl, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean): String = {
    val converter = if (occurrence.nillable) buildFromXML("scalaxb.DataRecord[Option[Any]]", "_")
      else buildFromXML(buildTypeName(XsAny), "_")
    
    buildParserString(if (mixed) "((any ^^ (" + converter + ")) ~ " + newline +
        indent(3) + buildTextParser + ") ^^ " + newline +
        indent(3) + "{ case p1 ~ p2 => Seq.concat(Seq(p1), p2.toList) }"
      else if (wrapInDataRecord) "(any ^^ (" + converter + "))"
      else "any",
      occurrence)
  }
  
  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildCompositorParser(compositor: HasParticle, occurrence: Occurrence, 
      mixed: Boolean, wrapInDataRecord: Boolean): String = compositor match {
    case ref: GroupRef        => buildGroupParser(buildGroup(ref), occurrence, mixed, wrapInDataRecord)
    case seq: SequenceDecl    => 
      if (containsSingleChoice(seq)) buildChoiceParser(singleChoice(seq), occurrence, mixed)
      else buildSeqParser(seq, occurrence, mixed, wrapInDataRecord)
    case choice: ChoiceDecl   => buildChoiceParser(choice, occurrence, mixed)
    case all: AllDecl         => buildAllParser(all, occurrence, mixed)
    case group: GroupDecl     => buildGroupParser(group, occurrence, mixed, wrapInDataRecord)
  }
  
  def buildGroupParser(group: GroupDecl, occurrence: Occurrence,
      mixed: Boolean, wrapInDataRecord: Boolean): String =
    buildParserString(if (mixed) "parsemixed" + groupTypeName(group)
      else "parse" + groupTypeName(group) + (if (wrapInDataRecord) "(true)" else ""),
      occurrence)
    
  // for unmixed wrapped in data record, this should generate Seq(DataRecord(None, None, Foo("1", "2")))
  // for mixed, this should generate
  // Seq(DataRecord(Some("ipo") Some("a"), "1"), DataRecord(None, None, "foo"), DataRecord(Some("ipo") Some("b"), "2"))
  def buildSeqParser(seq: SequenceDecl,
      occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean): String = {
    val parserList = if (mixed) (0 to seq.particles.size * 2 - 1).toList map { i =>
        if (i % 2 == 0) buildParser(seq.particles(i / 2), mixed, mixed)
        else buildTextParser
      }
      else seq.particles.map { buildParser(_, mixed, mixed) }
    
    def buildSeqConverter(seq: SequenceDecl, mixed: Boolean,
        wrapInDataRecord: Boolean): String = {
      lazy val name = makeTypeName(context.compositorNames(seq))
      val particles = buildParticles(seq)
      val parserVariableList = if (mixed) (0 to particles.size * 2 - 1) map { buildSelector }
        else (0 to particles.size - 1) map { buildSelector }
      val argList = if (mixed) (0 to particles.size * 2 - 1).toList map { i =>
          if (i % 2 == 0) buildArgForMixed(particles(i / 2), i)
          else buildArgForOptTextRecord(i) }
        else (0 to particles.size - 1).toList map { i => buildArg(particles(i), i) }
      val paramList = if (mixed) Nil
        else particles.map { buildParam }
      val hasSequenceParam = (paramList.size == 1) &&
        (paramList.head.cardinality == Multiple) &&
        (!mixed)

      def argsString = if (hasSequenceParam) argList.head + ": _*"
        else argList.mkString("," + newline + indent(4))

      "{ case " +
      parserVariableList.mkString(" ~ ") + 
      (if (mixed) " => Seq.concat(" + argsString + ")"
      else if (wrapInDataRecord) " => scalaxb.DataRecord(" + name + "(" + argsString + "))"
      else " => " + name + "(" + argsString + ")") +
      " }"
    }
    
    val base = parserList.mkString("(", " ~ " + newline + indent(3), ")") + " ^^ " + newline +
      indent(4) + buildSeqConverter(seq, mixed, wrapInDataRecord)
    val retval = buildParserString(base, occurrence)
    log("Parsers#buildSeqParser:  " + seq + newline + retval)
    retval
  }
  
  def buildAllParser(all: AllDecl, occurrence: Occurrence, mixed: Boolean): String = {
    val parserList = all.particles.map { buildParser(_, mixed, false) }
    
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
  def buildChoiceParser(choice: ChoiceDecl, occurrence: Occurrence, mixed: Boolean): String = {    
    val containsStructure = if (mixed) true
      else choice.particles exists(_ match {
        case elem: ElemDecl => false
        case ref: ElemRef => false
        case _ => true
        })
    val singleOccurrence = occurrence.copy(minOccurs = 1, maxOccurs = 1) 
    val parserList = choice.particles filterNot(
        _.isInstanceOf[AnyDecl]) map {
      case elem: ElemDecl =>
        if (mixed && containsStructure) buildParser(SequenceDecl(List(elem), 1, 1), singleOccurrence, mixed, true)
        else buildParser(elem, singleOccurrence, mixed, true)
      case ref: ElemRef =>
        if (mixed && containsStructure) buildParser(SequenceDecl(List(ref), 1, 1), singleOccurrence, mixed, true)
        else buildParser(ref, singleOccurrence, mixed, true)
      case particle => buildParser(particle, singleOccurrence, mixed, true)
    }
    val choiceOperator = if (containsStructure) "|||" else "|"
    
    val nonany = if (parserList.size > 0)
      parserList.mkString(" " + choiceOperator + " " + newline + indent(3))
    else ""
    
    val anyList = choice.particles filter(
      _.isInstanceOf[AnyDecl]) map { particle =>
      buildParser(particle, singleOccurrence, mixed, true) }
    val base = if (anyList.size > 0)
      if (nonany == "") anyList(0)
      else "(" + nonany + ") | " + newline +
        indent(3) + anyList(0)
    else nonany
    
    val retval = buildParserString(base, occurrence)
    log("Parsers#buildChoiceParser:  " + choice + newline + retval)
    retval
  }
    
  def buildSubstitionGroupParser(elem: ElemDecl, occurrence: Occurrence, mixed: Boolean): String = {
    log("Parsers#buildSubstitionGroupParser")    
    
    val particles = schema.topElems.valuesIterator.toList filter { x =>
      x.substitutionGroup map { sub => 
        elements(sub._1, sub._2) == elem
      } getOrElse { false }
    }
    val parserList = particles map { particle =>
      buildParser(particle, Occurrence(math.max(particle.minOccurs, 1), 1, occurrence.nillable), mixed, true)    
    }
    val choiceOperator = "|"
    val base = if (parserList.size > 0)
      parserList.mkString(" " + choiceOperator + " " + newline + indent(3))
    else ""
    
    buildParserString(base, occurrence)
  }
  
  def buildElemParser(elem: ElemDecl, occurrence: Occurrence, mixed: Boolean, wrapInDataRecord: Boolean): String = {
    def addConverter(p: String): String = if (wrapInDataRecord) "(" + p + " ^^ " + newline +
        indent(3) + buildConverter(elem.typeSymbol, occurrence) + ")"
      else p
    
    if ((isSubstitionGroup(elem))) addConverter(buildSubstitionGroupParser(elem, occurrence, mixed))
    else elem.typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => addConverter(buildParserString(elem, occurrence))
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  addConverter(buildParserString(elem, occurrence))
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (compositorWrapper.contains(decl)) {
          val compositor = compositorWrapper(decl)
          val o = buildOccurrence(compositor)
          buildCompositorParser(compositor, occurrence.copy(nillable = o.nillable), mixed, wrapInDataRecord)
        }
        else addConverter(buildParserString(elem, occurrence))
            
      case XsAny => addConverter(buildParserString("any", occurrence))
      case symbol: ReferenceTypeSymbol =>
        if (symbol.decl == null)
          error("Parsers#buildParser: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with null decl")
        else    
          error("Parsers#buildParser: " + elem.toString +
            " Invalid type " + symbol.getClass.toString + ": " +
            symbol.toString + " with " + symbol.decl.toString)
      case _ => error("Parsers#buildParser: " + elem.toString +
        " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
    }
  }
    
  def buildParserString(elem: ElemDecl, occurrence: Occurrence): String =
    buildParserString("scalaxb.ElemName(" +
      quoteNamespace(elem.namespace orElse schema.targetNamespace) + ", " +
      quote(elem.name) + ")",
      occurrence)
  
  def buildParserString(base: String, occurrence: Occurrence) =
    if (occurrence.maxOccurs > 1) "rep(" + base + ")"
    else if (occurrence.minOccurs == 0) "opt(" + base + ")"
    else "(" + base + ")"
  
  def buildConverter(typeSymbol: XsTypeSymbol, occurrence: Occurrence): String = {
    val arg = buildArg("x.node", typeSymbol)
    val nilRecord = "scalaxb.DataRecord(x.namespace, Some(x.name), None)"
    val someRecord = "scalaxb.DataRecord(x.namespace, Some(x.name), Some(" + arg + "))"
    val record = "scalaxb.DataRecord(x.namespace, Some(x.name), " + arg + ")"
    
    (toCardinality(occurrence), occurrence.nillable) match {
      case (Multiple, true)   => "(p => p.toSeq map { x => if (x.nil) " + nilRecord + " else " + someRecord + " })"        
      case (Multiple, false)  => "(p => p.toSeq map { x => " + record + " })" 
      case (Optional, true)   => "(p => p map { x => if (x.nil) " + nilRecord + " else " + someRecord + " })"
      case (Optional, false)  => "(p => p map { x => " + record + " })"      
      case (Single, true)     => "(x => if (x.nil) " + nilRecord + " else " + someRecord + ")"
      case (Single, false)    => "(x => " + record + ")"
    }
  }
  
  def buildParticles(com: Option[HasParticle], name: String): List[ElemDecl] = com match {
    case Some(c) => buildParticles(c)
    case None => Nil
  }

  def buildParticles(compositor: HasParticle): List[ElemDecl] =
    compositor.particles map {
      case ref: GroupRef            => buildCompositorRef(ref)        
      case seq: SequenceDecl        =>
        if (containsSingleChoice(seq)) buildCompositorRef(singleChoice(seq))
        else buildCompositorRef(seq)
      case compositor2: HasParticle => buildCompositorRef(compositor2)
      case elem: ElemDecl           => elem
      case ref: ElemRef             => buildElement(ref)
      case any: AnyDecl             => buildAnyRef(any)
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
    case compositor: HasParticle => Seq(buildCompositorRef(compositor).typeSymbol)
    case _                  => Nil  
  }
  
  def buildImplicitParams(dependents: List[Decl]) =
    immutable.ListMap[Decl, Param]((for (i <- 0 to dependents.size - 1)
      yield (dependents(i), 
        Param(None, "ev" + i, XsXMLFormat(dependents(i)), Single, false, false) )): _*)
  
  /// for recursive complex types, which would call this#toXML or this#fromXML.
  def buildFakeImplicitParam(decl: ComplexTypeDecl): Param =
    Param(None, "this", XsXMLFormat(decl), Single, false, false)
}
