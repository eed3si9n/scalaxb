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

trait Params extends Lookup {
  val ATTRS_PARAM = "attributes"
  var anyNumber = 0
  
  case class Occurrence(minOccurs: Int, maxOccurs: Int, nillable: Boolean)
  
  val SingleUnnillable = Occurrence(1, 1, false)
  
  abstract class Cardinality
  case object Optional extends Cardinality { override def toString: String = "Optional" }
  case object Single extends Cardinality { override def toString: String = "Single" }
  case object Multiple extends Cardinality { override def toString: String = "Multiple" }
  
  def toCardinality(minOccurs: Int, maxOccurs: Int): Cardinality =
    if (maxOccurs > 1) Multiple
    else if (minOccurs == 0) Optional
    else Single
    
  def toCardinality(occurrence: Occurrence): Cardinality =
    toCardinality(occurrence.minOccurs, occurrence.maxOccurs)
  
  def toCardinality(attr: AttributeDecl): Cardinality = {
    val minOccurs = if (attr.use == RequiredUse || 
        attr.fixedValue.isDefined || attr.defaultValue.isDefined) 1
      else 0
    toCardinality(minOccurs, 1)
  }
  
  case class Param(namespace: Option[String],
    name: String,
    typeSymbol: XsTypeSymbol,
    cardinality: Cardinality,
    nillable: Boolean,
    attribute: Boolean) {
    
    def baseTypeName: String = buildTypeName(typeSymbol)
    
    def typeName: String = cardinality match {
      case Single   =>
        if (nillable) "Option[" + baseTypeName + "]"
        else baseTypeName
      case Optional =>
        if (nillable) "Option[Option[" + baseTypeName + "]]"
        else "Option[" + baseTypeName + "]"
      case Multiple => 
        if (nillable) "Seq[Option[" + baseTypeName + "]]"
        else "Seq[" + baseTypeName + "]"
    }      
    
    def toScalaCode: String =
      makeParamName(name) + ": " + typeName
  }
  
  def buildParam(decl: Decl): Param = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case any: AnyAttributeDecl => buildParam(any)
    case group: AttributeGroupDecl => buildParam(group)
    case _ => error("Params#buildParam: unsupported delcaration " + decl.toString)
  }
  
  def buildParam(elem: ElemDecl): Param = {
    val nillable = elem.nillable getOrElse { false }
    val typeSymbol = if (isSubstitionGroup(elem)) buildSubstitionGroupSymbol(elem.typeSymbol)
      else elem.typeSymbol match {
        case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
          if (compositorWrapper.contains(decl)) buildCompositorSymbol(compositorWrapper(decl), elem.typeSymbol)
          else elem.typeSymbol
        case _ => elem.typeSymbol
      }
    
    val retval = if (nillable && typeSymbol == XsAny) Param(elem.namespace, elem.name, XsNillableAny, 
        toCardinality(elem.minOccurs, elem.maxOccurs), false, false)
      else if (typeSymbol == XsLongAttribute) Param(elem.namespace, elem.name, typeSymbol, 
          toCardinality(elem.minOccurs, elem.maxOccurs), nillable, true)
      else Param(elem.namespace, elem.name, typeSymbol, 
        toCardinality(elem.minOccurs, elem.maxOccurs), nillable, false)
    log("Params#buildParam:  " + retval.toString)
    retval
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val name = if (!attr.global) attr.name
      else makePrefix(attr.namespace, context) + attr.name
    
    val retval = Param(attr.namespace, name, attr.typeSymbol, toCardinality(attr), false, true)
    log("Params#buildParam:  " + retval.toString)
    retval
  }
  
  def buildParam(group: AttributeGroupDecl): Param = {
    val retval = Param(group.namespace, group.name,
      new AttributeGroupSymbol(group.namespace, group.name), Single, false, true)
    log("Params#buildParam:  " + retval.toString)
    retval    
  }
  
  def buildSubstitionGroupSymbol(typeSymbol: XsTypeSymbol): XsTypeSymbol =
    XsDataRecord(typeSymbol)
  
  def buildParam(any: AnyAttributeDecl): Param =
    Param(None, ATTRS_PARAM, XsAnyAttribute, Single, false, true)
  
  def buildCompositorSymbol(compositor: HasParticle, typeSymbol: XsTypeSymbol): XsTypeSymbol =
    compositor match {
      case ref: GroupRef =>
        buildCompositorSymbol(buildGroup(ref), typeSymbol)
      case group: GroupDecl =>
        val primary = primaryCompositor(group)
        val compositorRef = buildCompositorRef(primary)
        buildCompositorSymbol(primaryCompositor(group), compositorRef.typeSymbol)
      case seq: SequenceDecl => typeSymbol
      case _ => XsDataRecord(typeSymbol)    
    }
  
  /// called by makeGroup
  def buildParam(compositor: HasParticle): Param = {
    val elem = buildCompositorRef(compositor)
    val symbol = buildCompositorSymbol(compositor, elem.typeSymbol)
    
    Param(None, "arg1", symbol,
      toCardinality(compositor.minOccurs, compositor.maxOccurs), false, false)
  }
  
  def primaryCompositor(group: GroupDecl): HasParticle =
    if (group.particles.size == 1) group.particles(0) match {
      case seq: SequenceDecl    => 
        if (containsSingleChoice(seq)) singleChoice(seq)
        else seq
      case choice: ChoiceDecl   => choice
      case all: AllDecl         => all  
    }
    else error("Params#primaryCompositor: group must contain one content model: " + group)

  // context.compositorNames contains the definition of GroupDecl,
  // while particle GroupDecl may differ in cardinality.
  def groupTypeName(group: GroupDecl) =
    makeTypeName(context.compositorNames(groups(group.namespace, group.name)))
  
  def buildOccurrence(particle: Particle): Occurrence = particle match {
    case compositor: HasParticle => buildOccurrence(compositor)
    case elem: ElemDecl => Occurrence(elem.minOccurs, elem.maxOccurs, elem.nillable getOrElse {false})
    case ref: ElemRef   => Occurrence(ref.minOccurs, ref.maxOccurs,
      (ref.nillable getOrElse {false}) || (buildElement(ref).nillable getOrElse {false}))
    case any: AnyDecl   => Occurrence(any.minOccurs, any.maxOccurs, false)
  }
  
  def buildOccurrence(compos: HasParticle): Occurrence = compos match {
    case ref: GroupRef =>
      val o = buildOccurrence(buildGroup(ref))
      Occurrence(math.min(ref.minOccurs, o.minOccurs), math.max(ref.maxOccurs, o.maxOccurs), o.nillable)
    case group: GroupDecl =>
      val o = buildOccurrence(primaryCompositor(group))
      Occurrence(math.min(group.minOccurs, o.minOccurs), math.max(group.maxOccurs, o.maxOccurs), o.nillable)
    case choice: ChoiceDecl =>
      val minOccurs = (choice.minOccurs :: choice.particles.map(_.minOccurs)).min
      val maxOccurs = (choice.maxOccurs :: choice.particles.map(_.maxOccurs)).max
      val nillable = choice.particles exists {
        case elem: ElemDecl => elem.nillable getOrElse {false}
        case ref: ElemRef =>
          if (ref.nillable getOrElse {false}) true
          else buildElement(ref).nillable getOrElse {false}
        case _ => false
      }
      Occurrence(minOccurs, maxOccurs, nillable)
    case _ => Occurrence(compos.minOccurs, compos.maxOccurs, false)
  }
  
  def buildLongAllRef(all: AllDecl) =
    ElemDecl(Some(INTERNAL_NAMESPACE), "all", XsLongAll, None, None, 1, 1, None, None, None)
  
  def buildLongAttributeRef =
    ElemDecl(Some(INTERNAL_NAMESPACE), ATTRS_PARAM, XsLongAttribute, None, None, 1, 1, None, None, None)
  
  def buildAnyRef(any: AnyDecl) = {
    anyNumber += 1
    val name = if (anyNumber <= 1) "any"
      else "any" + anyNumber
    ElemDecl(Some(INTERNAL_NAMESPACE), name, XsAny, None, None, any.minOccurs, any.maxOccurs, None, None, None)
  }
  
  def buildCompositorRef(compositor: HasParticle): ElemDecl =
    buildCompositorRef(
      compositor match {
        case ref: GroupRef => buildGroup(ref)
        case _ => compositor
      },
      compositor match {
        // overriding nillable because nillable options are handled elsewhere.
        case choice: ChoiceDecl => buildOccurrence(compositor).copy(nillable = false)
        case _ => buildOccurrence(compositor)
      })

  def buildCompositorRef(compositor: HasParticle, occurrence: Occurrence): ElemDecl = {    
    val typeName = compositor match {
      case group: GroupDecl => groupTypeName(group)
      case _ => makeTypeName(context.compositorNames(compositor))
    }
    val name = typeName.toLowerCase
    
    val symbol = new ReferenceTypeSymbol(typeName)
    val decl = ComplexTypeDecl(schema.targetNamespace, symbol.name, symbol.name,
      false, false, ComplexContentDecl.empty, Nil, None)

    compositorWrapper(decl) = compositor

    symbol.decl = decl
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    typeNames(decl) = typeName
    
    ElemDecl(schema.targetNamespace, name, symbol, None, None,
      occurrence.minOccurs, occurrence.maxOccurs, Some(occurrence.nillable), None, None)
  }
  
  def buildChoiceTypeName(decl: ComplexTypeDecl, choice: ChoiceDecl,
      shortLocal: Boolean): String = 
    if (choice.particles.size < 1) "scalaxb.DataRecord[Any]"
    else {
      val firstParticle = choice.particles(0)
      
      def particleType(particle: Particle) = particle match {
        case elem: ElemDecl => Some(elem.typeSymbol)
        case ref: ElemRef => Some(buildElement(ref).typeSymbol)
        case _ => None
      }
      
      def sameType: Option[XsTypeSymbol] = {
        val firstType = particleType(firstParticle)
        if (firstType.isEmpty) None
        else if (choice.particles forall { particleType(_) == firstType }) firstType
        else None
      }
      
      def isOptionDescendant(particle: Particle): Boolean = particle match {
        case elem: ElemDecl =>
          elem.typeSymbol match {
            case ReferenceTypeSymbol(decl: ComplexTypeDecl) => true
            case _ => false
          }
        case ref: ElemRef =>
          buildElement(ref).typeSymbol match {
            case ReferenceTypeSymbol(decl: ComplexTypeDecl) => true
            case _ => false
          }
        case c: ChoiceDecl => c.particles forall { isOptionDescendant }
        case seq: SequenceDecl => true
        case _ => false
      }
      
      val member = sameType match {
        case Some(x) => buildTypeName(x)
        case None =>
          if (!containsForeignType(choice) &&
              (choice.particles forall { isOptionDescendant }) ) buildTypeName(decl, shortLocal)
          else "Any"
      }
      if (buildOccurrence(choice).nillable) "scalaxb.DataRecord[Option[" + member + "]]"
      else "scalaxb.DataRecord[" + member + "]"
    }
}
