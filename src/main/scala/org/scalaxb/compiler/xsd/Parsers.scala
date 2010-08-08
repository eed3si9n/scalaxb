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

package org.scalaxb.compiler.xsd
import scala.collection.mutable

trait Parsers extends Args with Params {
  // called by makeCaseClassWithType
  def buildParser(particle: Particle, mixed: Boolean, wrapInDataRecord: Boolean): String = particle match {
    case elem: ElemDecl       => buildParser(elem, elem.minOccurs, elem.maxOccurs, mixed)
    case ref: ElemRef         =>
      val elem = buildElement(ref)
      buildParser(elem, elem.minOccurs, elem.maxOccurs, mixed)
    case compositor: HasParticle =>
      buildParser(compositor, compositor.minOccurs, compositor.maxOccurs, mixed, wrapInDataRecord)
    case any: AnyDecl =>
      buildParser(any, any.minOccurs, any.maxOccurs)
  }

  def buildParser(any: AnyDecl, minOccurs: Int, maxOccurs: Int): String =
    buildParserString("any", minOccurs, maxOccurs)
  
  // minOccurs and maxOccurs may come from the declaration of the compositor,
  // or from the element declaration.
  def buildParser(compositor: HasParticle,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean, wrapInDataRecord: Boolean): String = compositor match {
    case ref: GroupRef        =>
      val group = buildGroup(ref)
      buildParser(group, minOccurs, maxOccurs, mixed, wrapInDataRecord)
    case seq: SequenceDecl    => 
      if (containsSingleChoice(seq)) buildParser(singleChoice(seq), minOccurs, maxOccurs, mixed)
      else buildParser(seq, minOccurs, maxOccurs, mixed, wrapInDataRecord)
    case choice: ChoiceDecl   => buildParser(choice, minOccurs, maxOccurs, mixed)
    case all: AllDecl         => buildParser(all, minOccurs, maxOccurs, mixed)
    case group: GroupDecl     => buildParser(group, minOccurs, maxOccurs, false, wrapInDataRecord)
  }
    
  def buildParser(group: GroupDecl, minOccurs: Int, maxOccurs: Int,
      mixed: Boolean, wrapInDataRecord: Boolean): String = {
    val compositor = primaryCompositor(group)
    buildParserString("parse" + groupTypeName(group) +
      (if (wrapInDataRecord) "(true)" else ""), 
      math.min(minOccurs, compositor.minOccurs),
      math.max(maxOccurs, compositor.maxOccurs) )
  }
  
  def buildParser(seq: SequenceDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean,
      wrapInDataRecord: Boolean): String = {
    val parserList = seq.particles.map { buildParser(_, mixed, false) }
    val base = parserList.mkString("(", " ~ " + newline + indent(2), ")") + " ^^ " + newline +
    indent(3) + buildConverter(seq, mixed, wrapInDataRecord)
    
    val retval = buildParserString(base, minOccurs, maxOccurs)
    log("GenSource#buildParser:  " + seq + newline + retval)
    retval
  }
  
  def buildParser(all: AllDecl, minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    val parserList = all.particles.map { buildParser(_, mixed, false) }
    val base = parserList.mkString("(", " ~ " + newline + indent(2), ")") + " ^^ " + newline +
    indent(3) + buildConverter(all)
    buildParserString(base, minOccurs, maxOccurs)   
  }
  
  // one or more particles may be emptiable within the choices.
  // in such case, treat the whole choice to be minOccurs = 0,
  // but make sure all particles has at least minOccurs = 1.
  // additionally, treat all particles as maxOccurs = 1 and make the whole
  // choice repeatable in case any one particle is repeatable.
  // this may violate the schema, but it is a compromise as long as plurals are
  // treated as Seq[DataRecord].
  def buildParser(choice: ChoiceDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = {
    def buildChoiceParser(particle: Particle): String = particle match {
      case ref: GroupRef        =>
        val group = buildGroup(ref)
        buildParser(group, math.max(group.minOccurs, 1), 1, false, true)
      case compositor: HasParticle =>
        buildParser(compositor, math.max(compositor.minOccurs, 1), 1, mixed, true)
      case elem: ElemDecl       =>
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1, mixed) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
      case any: AnyDecl         =>
        "(" + buildParser(any, math.max(any.minOccurs, 1), 1) + " ^^ " + newline +
        indent(3) + buildConverter(XsAny, math.max(any.minOccurs, 1), 1) + ")"
      case ref: ElemRef         =>
        val elem = buildElement(ref)
        "(" + buildParser(elem, math.max(elem.minOccurs, 1), 1, mixed) + " ^^ " + newline +
        indent(3) + buildConverter(elem, math.max(elem.minOccurs, 1), 1) + ")"
    }
    
    val parserList = choice.particles filterNot(
      _.isInstanceOf[AnyDecl]) map(buildChoiceParser(_))
    val choiceOperator = if (choice.particles exists(_ match {
      case elem: ElemDecl => false
      case ref: ElemRef => false
      case _ => true
      })) "|||"
    else "|"
      
    val nonany = if (parserList.size > 0)
      parserList.mkString(" " + choiceOperator + " " + newline + indent(2))
    else ""
    
    val anyList = choice.particles filter(
      _.isInstanceOf[AnyDecl]) map(buildChoiceParser(_))
    val base = if (anyList.size > 0)
      if (nonany == "") anyList(0)
      else "(" + nonany + ") | " + newline +
        indent(2) + anyList(0)
    else nonany
    
    buildParserString(base, minOccurs, maxOccurs)
  }
  
  def buildParser(elem: ElemDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String = elem.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildParserString(elem, minOccurs, maxOccurs)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildParser(elem, decl, minOccurs, maxOccurs, mixed)
    case XsAny => buildParserString("any", minOccurs, maxOccurs)
    case symbol: ReferenceTypeSymbol =>
      if (symbol.decl == null)
        error("GenSource#buildParser: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with null decl")
      else    
        error("GenSource#buildParser: " + elem.toString +
          " Invalid type " + symbol.getClass.toString + ": " +
          symbol.toString + " with " + symbol.decl.toString)
    case _ => error("GenSource#buildParser: " + elem.toString +
      " Invalid type " + elem.typeSymbol.getClass.toString + ": " + elem.typeSymbol.toString)
  }
  
  def buildParser(elem: ElemDecl, decl: ComplexTypeDecl,
      minOccurs: Int, maxOccurs: Int, mixed: Boolean): String =
    if (compositorWrapper.contains(decl)) {
      val compositor = compositorWrapper(decl)
      buildParser(compositor, minOccurs, maxOccurs, mixed, false)
    }
    else buildParserString(elem, minOccurs, maxOccurs)
  
  def buildParserString(elem: ElemDecl, minOccurs: Int, maxOccurs: Int): String =
    buildParserString("rt.ElemName(" + quoteNamespace(elem.namespace) + ", " + quote(elem.name) + ")",
      minOccurs, maxOccurs)
  
  def buildParserString(base: String, minOccurs: Int, maxOccurs: Int) =
    if (maxOccurs > 1) "rep(" + base + ")"
    else if (minOccurs == 0) "opt(" + base + ")"
    else "(" + base + ")"
    
  def buildConverter(seq: SequenceDecl, mixed: Boolean,
      wrapInDataRecord: Boolean): String = {
    val name = makeTypeName(context.compositorNames(seq))
    val particles = buildParticles(seq)
    val parserVariableList =  for (i <- 0 to particles.size - 1)
      yield "p" + (i + 1)

    val argList = (for (i <- 0 to particles.size - 1)
      yield buildArg(particles(i), i) ).toList

    val paramList = particles.map { buildParam }
    val hasSequenceParam = (paramList.size == 1) &&
      (paramList.head.cardinality == Multiple) &&
      (!paramList.head.attribute) &&
      (!mixed)

    def argsString = if (hasSequenceParam) argList.head + ": _*"
    else argList.mkString("," + newline + indent(3))

    "{ case " +
    parserVariableList.mkString(" ~ " + newline + indent(3)) + 
    (if (wrapInDataRecord) " => rt.DataRecord(None, None, " + name + "(" + argsString + "))"
    else " => " + name + "(" + argsString + ")") +
    " }"
  }

  def buildConverter(seq: AllDecl): String = {
    "{ case p => foo()}"
  }

  def buildConverter(elem: ElemDecl): String =
    buildConverter(elem, elem.minOccurs, elem.maxOccurs)

  def buildConverter(elem: ElemDecl, minOccurs: Int, maxOccurs: Int): String =
    buildConverter(elem.typeSymbol, minOccurs, maxOccurs)

  def buildConverter(typeSymbol: XsTypeSymbol, minOccurs: Int, maxOccurs: Int): String =
    if (maxOccurs > 1)
      "(p => p.toList map(x => rt.DataRecord(x.namespace, Some(x.name), " +
      buildArg("x.node", typeSymbol) + ")))"
    else if (minOccurs == 0)
      "(p => p map { x =>" + newline +
      indent(3) + "rt.DataRecord(x.namespace, Some(x.name), " +
      buildArg("x.node", typeSymbol) + ") })"
    else
      "(x => rt.DataRecord(x.namespace, Some(x.name), " + buildArg("x.node", typeSymbol) + "))"

  def buildParticles(com: Option[HasParticle], name: String): List[ElemDecl] = com match {
    case Some(c) => buildParticles(c)
    case None => Nil
  }

  def buildParticles(compositor: HasParticle): List[ElemDecl] =
    compositor.particles map {
      case ref: GroupRef            =>
        val group = buildGroup(ref)
        buildCompositorRef(primaryCompositor(group))        
      case seq: SequenceDecl        =>
        if (containsSingleChoice(seq)) buildCompositorRef(singleChoice(seq))
        else buildCompositorRef(seq)
      case compositor2: HasParticle => buildCompositorRef(compositor)
      case elem: ElemDecl           => elem
      case ref: ElemRef             => buildElement(ref)
      case any: AnyDecl             => buildAnyRef(any)
    }
    
  def buildAnyRef(any: AnyDecl) =
    ElemDecl(Some(INTERNAL_NAMESPACE), "any", XsAny, None, None,
      any.minOccurs, any.maxOccurs, None, None)

}
