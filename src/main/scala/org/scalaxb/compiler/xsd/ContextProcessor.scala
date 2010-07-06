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

import org.scalaxb.compiler.{ScalaNames, Logger}
import scala.collection.mutable

class ContextProcessor(logger: Logger) extends ScalaNames {
  
  def packageName(schema: SchemaDecl, context: XsdContext): Option[String] =
    packageName(schema.targetNamespace, context)

  def packageName(decl: ComplexTypeDecl, context: XsdContext): Option[String] =
    packageName(decl.namespace, context)
      
  def packageName(namespace: String, context: XsdContext): Option[String] =
    if (context.packageNames.contains(namespace)) context.packageNames(namespace)
    else if (context.packageNames.contains(null)) context.packageNames(null)
    else None
      
  def processContext(context: XsdContext,
      packageNames: collection.Map[String, Option[String]]) {
    context.packageNames ++= packageNames
    packageNames.valuesIterator.toList.distinct.map(
      pkg => context.typeNames(pkg) = mutable.ListMap.empty[ComplexTypeDecl, String]
      )
    context.typeNames(None) = mutable.ListMap.empty[ComplexTypeDecl, String]
    
    val anonymousTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]
    
    for (schema <- context.schemas;
        elem <- schema.elemList;
        val typeSymbol = elem.typeSymbol;
        if typeSymbol.name.contains("@");
        if typeSymbol.isInstanceOf[ReferenceTypeSymbol];
        val ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol];
        if ref.decl.isInstanceOf[ComplexTypeDecl];
        val decl = ref.decl.asInstanceOf[ComplexTypeDecl]) {
      val pair = (schema, decl)
      anonymousTypes += pair
      val typeNames = context.typeNames(packageName(schema, context))
      val nameCandidate = makeTypeName(elem.name)
      if (!typeNames.valuesIterator.contains(nameCandidate))
        typeNames(decl) = nameCandidate
      else {
        var i = 2
        while (typeNames.valuesIterator.contains(nameCandidate + i) || i > 100) {
          i += 1
        }
        typeNames(decl) = nameCandidate + i
      }
    }
    
    val namedTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]
    
    for (schema <- context.schemas;
        typ <- schema.typeList;
        if typ.isInstanceOf[ComplexTypeDecl];
        val decl = typ.asInstanceOf[ComplexTypeDecl];
        if !decl.name.contains("@")) {    
      val pair = (schema, decl)
      namedTypes += pair
      val typeNames = context.typeNames(packageName(schema, context))
      typeNames(decl) = makeProtectedTypeName(decl, context)
    }
    
    context.complexTypes ++= anonymousTypes.toList.distinct :::
      namedTypes.toList.distinct
      
    for (schema <- context.schemas;
        group <- schema.topGroups.valuesIterator.toList) {
      val pair = (schema, group)
      context.groups += pair
    }
        
    def associateSubType(subType: ComplexTypeDecl, base: ComplexTypeDecl) {
      if (context.baseToSubs.contains(base))
        context.baseToSubs(base) = subType :: context.baseToSubs(base)
      else
        context.baseToSubs(base) = subType :: Nil
    }
    
    for ((schema, typ) <- context.complexTypes)  typ.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, base)
      case _ =>
    }
    
    for (base <- context.baseToSubs.keysIterator;
        if !base.abstractValue) {
      val typeNames = context.typeNames(packageName(base.namespace, context))
      typeNames(base) = makeTraitName(base)
    }
    
    makeCompositorNames(context)
  }
  
  def makeGroupComplexType(group: GroupDecl) =
    ComplexTypeDecl(group.namespace, group.name, false, false,
      ComplexContentDecl.empty, Nil, None)

  def containsSingleChoice(seq: SequenceDecl) = seq.particles match {
    case ChoiceDecl(_, _, _, _) :: Nil => true
    case _ => false
  }

  def singleChoice(seq: SequenceDecl): ChoiceDecl = seq.particles match {
    case (choice@ChoiceDecl(_, _, _, _)) :: Nil => choice
    case _ => error("Does not cointain single choice.")
  }
  
  def makeCompositorNames(context: XsdContext) {
    var sequenceNumber = 0
    var choiceNumber = 0
    var allNumber = 0
    
    for ((schema, decl) <- context.complexTypes) { 
      sequenceNumber = 0
      choiceNumber = 0
      allNumber = 0
      decl.content.content match {
        case CompContRestrictionDecl(_, Some(compositor: HasParticle), _) =>
          makeCompositorName(compositor, decl)
        case CompContExtensionDecl(_, Some(compositor: HasParticle), _) =>
          makeCompositorName(compositor, decl)
        case _ =>
      }
    }
    
    for ((schema, group) <- context.groups) {
      sequenceNumber = 0
      choiceNumber = 0
      allNumber = 0
      
      context.compositorNames(group) = group.name + "Group"
      if (group.particles.size == 1) group.particles(0) match {
        case compositor: HasParticle => makeGroupCompositorName(compositor, group)
      }
      else error("ContextProcessor#makeCompositorNames: group must contain one content model: " + group)
    }
    
    def isFirstCompositor =
      (sequenceNumber + choiceNumber + allNumber == 0)
        
    def makeGroupCompositorName(compositor: HasParticle, group: GroupDecl) {
      val groupName = group.name
      
      compositor match {
        case seq@SequenceDecl(particles: List[_], _, _) =>
          if (!isFirstCompositor ||
              !containsSingleChoice(seq))
            context.compositorParents(compositor) = makeGroupComplexType(group)
          
          if (isFirstCompositor) context.compositorNames(compositor) = groupName + "Sequence"
          else context.compositorNames(compositor) = groupName + "Sequence" + (sequenceNumber + 1)
          sequenceNumber += 1
             
        case ChoiceDecl(particles: List[_], _, _, _) =>
          context.compositorParents(compositor) = makeGroupComplexType(group)
          if (isFirstCompositor) context.compositorNames(compositor) = groupName + "Option"
          else context.compositorNames(compositor) = groupName + "Option" + (choiceNumber + 1)
          choiceNumber += 1
          
        case AllDecl(particles: List[_], _, _) =>
          context.compositorParents(compositor) = makeGroupComplexType(group)
          if (isFirstCompositor) context.compositorNames(compositor) = groupName + "All"
          else context.compositorNames(compositor) = groupName + "All" + (allNumber + 1)
          allNumber += 1
        
        case _ =>
      }
      
      compositor.particles collect {
        case compositor2: HasParticle => makeGroupCompositorName(compositor2, group)
      }      
    }
    
    def makeCompositorName(compositor: HasParticle, decl: ComplexTypeDecl) {
      val typeNames = context.typeNames(packageName(decl.namespace, context))
      val typeName = typeNames(decl)
      
      compositor match {
        case SequenceDecl(particles: List[_], _, _) =>
          if (isFirstCompositor)
            context.compositorNames(compositor) = typeName
          else {
            context.compositorNames(compositor) = typeName + "Sequence" + (sequenceNumber + 1)
            context.compositorParents(compositor) = decl
          }
          sequenceNumber += 1
             
        case ChoiceDecl(particles: List[_], _, _, _) =>
          context.compositorParents(compositor) = decl
          if (choiceNumber == 0)
            context.compositorNames(compositor) = typeName + "Option"
          else
            context.compositorNames(compositor) = typeName + "Option" + (choiceNumber + 1)
          choiceNumber += 1
          
        case AllDecl(particles: List[_], _, _) =>
          context.compositorParents(compositor) = decl
          if (allNumber == 0)
            context.compositorNames(compositor) = typeName + "All"
          else
            context.compositorNames(compositor) = typeName + "All" + (allNumber + 1)
          allNumber += 1
          
        case _ =>
      }
      
      compositor.particles collect {
        case compositor2: HasParticle => makeCompositorName(compositor2, decl)
      }
    } // makeCompositorName
  }
    
  def makeProtectedTypeName(decl: ComplexTypeDecl, context: XsdContext): String = {
    val typeNames = context.typeNames(packageName(decl.namespace, context))
    if (!typeNames.valuesIterator.contains(decl.name))
      makeTypeName(decl.name)
    else {
      var name = makeTypeName(decl.name)  + "Type"
      for (i <- 2 to 100) {
        if (typeNames.valuesIterator.contains(name))
          name = makeTypeName(decl.name)  + "Type" + i
      }
      name
    }
  }
  
  def makeTraitName(decl: ComplexTypeDecl) =
    if (decl.name.last == 'e')
      makeTypeName(decl.name.dropRight(1) + "able")
    else makeTypeName(decl.name + "able")
  
  def makeTypeName(name: String) = name match {
    case "javax.xml.datatype.Duration" => name
    case "java.util.GregorianCalendar" => name
    case "java.net.URI" => name
    case "javax.xml.namespace.QName" => name
    case _ =>
      val base = identifier(name).capitalize
      if (isCommonlyUsedWord(base)) base + "Type"
      else base
  }
  
  def makeParamName(name: String) =
    if (isKeyword(name)) name + "Value"
    else identifier(name)
  
  def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")
}
