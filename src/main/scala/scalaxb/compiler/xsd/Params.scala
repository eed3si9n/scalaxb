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
  val ANY_ATTR_PARAM = "anyAttribute"
  var argNumber = 0
  
  abstract class Cardinality
  case object Optional extends Cardinality
  case object Single extends Cardinality
  case object Multiple extends Cardinality
  
  def toCardinality(minOccurs: Int, maxOccurs: Int) =
    if (maxOccurs > 1) Multiple
    else if (minOccurs == 0) Optional
    else Single
  
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
      case Optional => "Option[" + baseTypeName + "]"
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
    case _ => error("GenSource#buildParam: unsupported delcaration " + decl.toString)
  }
  
  def buildParam(elem: ElemDecl): Param = {
    val typeSymbol = if (isSubstitionGroup(elem)) buildSubstitionGroupSymbol(elem.typeSymbol)
    else elem.typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        if (compositorWrapper.contains(decl))
          buildCompositorSymbol(compositorWrapper(decl), elem.typeSymbol)
        else elem.typeSymbol
      case _ => elem.typeSymbol
    }
    val nillable = elem.nillable getOrElse { false }
    val retval = Param(elem.namespace, elem.name, typeSymbol, 
      toCardinality(elem.minOccurs, elem.maxOccurs), nillable, false)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(attr: AttributeDecl): Param = {
    val cardinality = if (toMinOccurs(attr) == 0) Optional
    else Single
    val name = if (!attr.global) attr.name
    else Option[String](schema.scope.getPrefix(attr.namespace.orNull)).
      getOrElse("") + attr.name
    
    val retval = Param(attr.namespace, name, attr.typeSymbol, cardinality, false, true)
    log("GenSource#buildParam:  " + retval)
    retval
  }
  
  def buildParam(group: AttributeGroupDecl): Param = {
    val retval = Param(group.namespace, group.name,
      new AttributeGroupSymbol(group.namespace, group.name), Single, false, true)
    log("GenSource#buildParam:  " + retval)
    retval    
  }
  
  def buildSubstitionGroupSymbol(typeSymbol: XsTypeSymbol): XsTypeSymbol =
    XsDataRecord(typeSymbol)
  
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
  
  def buildParam(any: AnyAttributeDecl): Param =
    Param(None, ANY_ATTR_PARAM, XsAnyAttribute, Multiple, false, true)

  def toMinOccurs(attr: AttributeDecl) = 
    if (attr.use == RequiredUse ||
      attr.fixedValue.isDefined ||
      attr.defaultValue.isDefined) 1
    else 0
  
  def primaryCompositor(group: GroupDecl): HasParticle =
    if (group.particles.size == 1) group.particles(0) match {
      case seq: SequenceDecl    => 
        if (containsSingleChoice(seq)) singleChoice(seq)
        else seq
      case choice: ChoiceDecl   => choice
      case all: AllDecl         => all  
    }
    else error("GenSource#primaryCompositor: group must contain one content model: " + group)

  // context.compositorNames contains the definition of GroupDecl,
  // while particle GroupDecl may differ in cardinality.
  def groupTypeName(group: GroupDecl) =
    makeTypeName(context.compositorNames(groups(group.namespace, group.name)))

  def buildCompositorRef(compositor: HasParticle): ElemDecl = {
    val minOccurs = compositor match {
      case ref: GroupRef =>
        val group = buildGroup(ref)
        val primary = primaryCompositor(group)
        List(ref.minOccurs, group.minOccurs, primary.minOccurs).min
      case group: GroupDecl =>
        val primary = primaryCompositor(group)
        math.min(group.minOccurs, primary.minOccurs)
      case choice: ChoiceDecl => (compositor.minOccurs :: compositor.particles.map(_.minOccurs)).min
      case _ => compositor.minOccurs
    }
    
    val maxOccurs = compositor match {
      case ref: GroupRef =>
        val group = buildGroup(ref)
        val primary = primaryCompositor(group)
        List(ref.maxOccurs, group.maxOccurs, primary.maxOccurs).max  
      case group: GroupDecl =>
        val primary = primaryCompositor(group)
        math.max(group.maxOccurs, primary.maxOccurs)
      case choice: ChoiceDecl => (compositor.maxOccurs :: compositor.particles.map(_.maxOccurs)).max
      case _ => compositor.maxOccurs
    }
    
    buildCompositorRef(
      compositor match {
        case ref: GroupRef => buildGroup(ref)
        case _ => compositor
      },
      minOccurs, maxOccurs)
  }

  def buildCompositorRef(compositor: HasParticle, minOccurs: Int, maxOccurs: Int): ElemDecl = {    
    argNumber += 1
    val name = "arg" + argNumber 

    val typeName = compositor match {
      case group: GroupDecl => groupTypeName(group)
      case _ => makeTypeName(context.compositorNames(compositor))
    }
    val symbol = new ReferenceTypeSymbol(typeName)
    val decl = ComplexTypeDecl(schema.targetNamespace, symbol.name, symbol.name,
      false, false, ComplexContentDecl.empty, Nil, None)

    compositorWrapper(decl) = compositor

    symbol.decl = decl
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    typeNames(decl) = typeName

    ElemDecl(schema.targetNamespace, name, symbol, None, None, minOccurs, maxOccurs,
      None, None, None)
  }
}
