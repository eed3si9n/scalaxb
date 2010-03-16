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
  
  def packageName(namespace: String, context: XsdContext): Option[String] =
    if (context.packageNames.contains(namespace))
      context.packageNames(namespace)
    else
      context.packageNames(null)
  
  def processContext(context: XsdContext,
      packageNames: Map[String, Option[String]]) {
    context.packageNames ++= packageNames
    packageNames.valuesIterator.toList.removeDuplicates.map(
      pkg => context.typeNames(pkg) = mutable.ListMap.empty[ComplexTypeDecl, String]
      )
    
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
        typePair <- schema.types;
        if typePair._2.isInstanceOf[ComplexTypeDecl];
        if !typePair._1.contains("@");
        val decl = typePair._2.asInstanceOf[ComplexTypeDecl]) {    
      val pair = (schema, decl)
      namedTypes += pair
      val typeNames = context.typeNames(packageName(schema, context))
      typeNames(decl) = makeProtectedTypeName(decl, context)
    }
    
    context.complexTypes ++= anonymousTypes.toList.removeDuplicates :::
      namedTypes.toList.removeDuplicates
    
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
    
    for (base <- context.baseToSubs.keysIterator) {
      val typeNames = context.typeNames(packageName(base.namespace, context))
      typeNames(base) = makeTraitName(base)
    }
    
    makeChoiceNames(context)
  }
  
  def makeChoiceNames(context: XsdContext) {
    var choiceNumber = 0
    
    for ((schema, decl) <- context.complexTypes) { 
      choiceNumber = 0
      val typeNames = context.typeNames(packageName(decl.namespace, context))
      decl.content.content match {
        case CompContRestrictionDecl(_, Some(compositor: HasParticle), _) =>
          makeChoiceName(compositor, typeNames(decl))
        case CompContExtensionDecl(_, Some(compositor: HasParticle), _) =>
          makeChoiceName(compositor, typeNames(decl))
        case _ =>
      }
    }
    
    def makeChoiceName(compositor: HasParticle, name: String): Unit = compositor match {
      case SequenceDecl(particles: List[_], _, _) =>
        var index = 0
        for (particle <- particles) {
          particle match {
            case choice: ChoiceDecl =>
              makeChoiceName(choice, name)
              context.choicePositions(choice) = index
            case compositor2: HasParticle => makeChoiceName(compositor2, name)
            case _ =>
          }
          index += 1
        }

      case AllDecl(particles: List[_], _, _) =>
        for (particle <- particles)
          particle match {
            case compositor2: HasParticle => makeChoiceName(compositor2, name)
            case _ =>
          }

      case choice@ChoiceDecl(particles: List[_], _, _, _) =>
        if (choiceNumber == 0)
          context.choiceNames(choice) = name + "Option"
        else
          context.choiceNames(choice) = name + "Option" + (choiceNumber + 1)
        choiceNumber += 1
        context.choicePositions(choice) = 0

        for (particle <- particles)
          particle match {
            case compositor2: HasParticle => makeChoiceName(compositor2, name)
            case _ =>
          }
    }
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
      decl.name.dropRight(1) + "able"
    else
      decl.name + "able"
  
  def makeTypeName(name: String) =
    if (name.contains("."))
      name
    else
      identifier(name).capitalize
  
  def makeParamName(name: String) =
    if (isKeyword(name))
      name + "Value"
    else
      identifier(name)
  
  def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")
}
