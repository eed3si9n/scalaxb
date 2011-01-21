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

import scalaxb.compiler.{Logger, ReferenceNotFound}
import scala.collection.mutable

trait Lookup extends ContextProcessor {
  def logger: Logger
  def schema: SchemaDecl
  def context: XsdContext
  
  val schemas = context.schemas.toList
  val compositorWrapper = mutable.ListMap.empty[ComplexTypeDecl, HasParticle]
  val INTERNAL_NAMESPACE = "http://scalaxb.org/internal"
  
  def elements(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     => throw new ReferenceNotFound("element" , namespace, name)
      }  
  
  def buildElement(ref: ElemRef) = {
    val that = elements(ref.namespace, ref.name)

    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    ElemDecl(that.namespace, that.name, that.typeSymbol, that.defaultValue,
      that.fixedValue, ref.minOccurs, ref.maxOccurs, ref.nillable match {
        case None => that.nillable
        case _    => ref.nillable
      },
      that.substitutionGroup, that.annotation)
  }

  def buildElement(base: BuiltInSimpleTypeSymbol): ElemDecl = 
    ElemDecl(schema.targetNamespace, "value", base, None, None, 1, 1, None, None, None)

  def groups(namespace: Option[String], name: String) =
    (for (schema <- schemas;
         if schema.targetNamespace == namespace;
         if schema.topGroups.contains(name))
       yield schema.topGroups(name)) match {
       case x :: xs => x
       case Nil     => throw new ReferenceNotFound("group" , namespace, name)
    }

  def buildGroup(ref: GroupRef) = {
    val that = groups(ref.namespace, ref.name)

    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    GroupDecl(that.namespace, that.name, that.particles,
      ref.minOccurs, ref.maxOccurs, that.annotation)   
  }
  
  lazy val xmlAttrs = Map[String, AttributeDecl](
    ("lang" -> AttributeDecl(Some(XML_URI), "lang", XsString, None, None, OptionalUse, None, true)),
    ("space" -> AttributeDecl(Some(XML_URI), "space", XsString, None, None, OptionalUse, None, true)),
    ("base" -> AttributeDecl(Some(XML_URI), "base", XsAnyURI, None, None, OptionalUse, None, true)),
    ("id" -> AttributeDecl(Some(XML_URI), "id", XsID, None, None, OptionalUse, None, true))
  )
  
  def attrs(namespace: Option[String], name: String) =
    if (namespace == Some(XML_URI)) xmlAttrs(name)
    else
      (for (schema <- schemas;
            if schema.targetNamespace == namespace;
            if schema.topAttrs.contains(name))
          yield schema.topAttrs(name)) match {
          case x :: xs => x
          case Nil     => throw new ReferenceNotFound("attribute" , namespace, name)
        }
  
  def buildAttribute(ref: AttributeRef) = {
    val that = attrs(ref.namespace, ref.name)
    // http://www.w3.org/TR/xmlschema-0/#Globals
    // In other words, global declarations cannot contain the attributes
    // minOccurs, maxOccurs, or use.
    AttributeDecl(that.namespace, that.name, that.typeSymbol,
      ref.defaultValue, ref.fixedValue, ref.use, that.annotation, that.global)
  }
  
  def attributeGroups(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topAttrGroups.contains(name))
        yield schema.topAttrGroups(name)) match {
        case x :: xs => x
        case Nil     => throw new ReferenceNotFound("attribute group" , namespace, name)
      }
      
  def buildAttributeGroup(ref: AttributeGroupRef) =
    attributeGroups(ref.namespace, ref.name)
  
  def buildTypeName(typeSymbol: XsTypeSymbol, shortLocal: Boolean = false): String = typeSymbol match {
    case XsAnyType       => "scalaxb.DataRecord[Any]"
    case XsNillableAny   => "scalaxb.DataRecord[Option[Any]]"
    case XsLongAll       => "Map[String, scalaxb.DataRecord[Any]]"
    case XsLongAttribute => "Map[String, scalaxb.DataRecord[Any]]"
    case XsAnyAttribute  => "Map[String, scalaxb.DataRecord[Any]]"
    case XsDataRecord(ReferenceTypeSymbol(decl: ComplexTypeDecl)) if compositorWrapper.contains(decl) =>
      compositorWrapper(decl) match {
        case choice: ChoiceDecl => buildChoiceTypeName(decl, choice, shortLocal)
        case _ => "scalaxb.DataRecord[Any]"
      }
    case r: XsDataRecord => "scalaxb.DataRecord[Any]"
    case XsMixed         => "scalaxb.DataRecord[Any]"
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl, shortLocal)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl, shortLocal)
    case symbol: AttributeGroupSymbol => buildTypeName(attributeGroups(symbol.namespace, symbol.name), shortLocal)
    case XsXMLFormat(decl: ComplexTypeDecl) => "scalaxb.XMLFormat[" + buildTypeName(decl, shortLocal) + "]"
    case XsXMLFormat(group: AttributeGroupDecl) => "scalaxb.XMLFormat[" + buildTypeName(group, shortLocal) + "]"
  }
  
  def buildChoiceTypeName(decl: ComplexTypeDecl, choice: ChoiceDecl, shortLocal: Boolean): String
  
  def xmlFormatTypeName(decl: ComplexTypeDecl): String =
    "scalaxb.XMLFormat[" + buildTypeName(decl, false) + "]"
  
  def buildTypeName(decl: ComplexTypeDecl, shortLocal: Boolean): String =
    buildTypeName(packageName(decl, context), decl, shortLocal)
    
  def buildEnumTypeName(decl: SimpleTypeDecl, shortLocal: Boolean): String =
    buildTypeName(packageName(decl, context), decl, shortLocal)
  
  def buildTypeName(pkg: Option[String], decl: Decl, shortLocal: Boolean): String = {
    val typeNames = context.typeNames(pkg)
    if (!typeNames.contains(decl)) error(pkg + ": Type name not found: " + decl.toString)
    
    if (shortLocal && pkg == packageName(schema, context)) typeNames(decl)
    else buildFullyQualifiedName(pkg, typeNames(decl))    
  }
  
  def buildTypeName(decl: SimpleTypeDecl, shortLocal: Boolean): String = decl.content match {
    case x@SimpTypRestrictionDecl(_, _) if containsEnumeration(decl)  => buildEnumTypeName(decl, shortLocal)
    case x: SimpTypRestrictionDecl                                    => buildTypeName(baseType(decl), shortLocal)
    case SimpTypListDecl(ReferenceTypeSymbol(itemType: SimpleTypeDecl)) if containsEnumeration(itemType) =>
      "Seq[" + buildEnumTypeName(itemType, shortLocal) + "]"
    case x: SimpTypListDecl => "Seq[" + buildTypeName(baseType(decl), shortLocal) + "]"
    case x: SimpTypUnionDecl => buildTypeName(baseType(decl), shortLocal)
  }
  
  def buildTypeName(group: AttributeGroupDecl, shortLocal: Boolean): String =
    buildTypeName(packageName(group, context), group, shortLocal)
  
  def buildTypeName(enumTypeName: String, enum: EnumerationDecl, shortLocal: Boolean): String = {
    val pkg = packageName(schema, context)
    val typeNames = context.enumValueNames(pkg)
    if (!typeNames.contains(enumTypeName, enum))
      error(pkg + ": Type name not found: " + enum.toString)
    
    if (shortLocal && pkg == packageName(schema, context)) typeNames(enumTypeName, enum)
    else buildFullyQualifiedName(pkg, typeNames(enumTypeName, enum))   
  }
  
  def buildFullyQualifiedName(sch: SchemaDecl, localName: String): String =
    buildFullyQualifiedName(packageName(sch, context), localName)
  
  def buildFullyQualifiedName(pkg: Option[String], localName: String): String =
    pkg.map(_ + ".").getOrElse("") + localName
  
  def buildFormatterName(group: AttributeGroupDecl): String =
    buildFormatterName(group.namespace, buildTypeName(group, true))
  
  def buildFormatterName(namespace: Option[String], name: String): String = {
    val pkg = packageName(namespace, context) getOrElse {""}
    val lastPart = pkg.split('.').reverse.head
    
    lastPart.capitalize + name + "Format"
  }
    
  def baseType(decl: SimpleTypeDecl): BuiltInSimpleTypeSymbol = decl.content match {
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol, _) => base
    case SimpTypRestrictionDecl(ReferenceTypeSymbol(decl2@SimpleTypeDecl(_, _, _, _, _)), _) => baseType(decl2)
    case SimpTypListDecl(itemType: BuiltInSimpleTypeSymbol) => itemType
    case SimpTypListDecl(ReferenceTypeSymbol(decl2@SimpleTypeDecl(_, _, _, _, _))) => baseType(decl2)
    case SimpTypUnionDecl() => XsString
    
    case _ => error("GenSource: Unsupported content " +  decl.content.toString)
  }
  
  def containsForeignType(compositor: HasParticle) =
    compositor.particles.exists(_ match {
        case ref: ElemRef => ref.namespace != schema.targetNamespace
        case ref: GroupRef => ref.namespace != schema.targetNamespace
        case _ => false
      }
    )
    
  def isSubstitionGroup(elem: ElemDecl) =
    elem.namespace map { x =>
      context.substituteGroups.contains((elem.namespace, elem.name))
    } getOrElse { false }
  
  def quoteNamespace(namespace: Option[String]) =
    if (namespace == schema.targetNamespace) "targetNamespace"
    else quote(namespace)
}
 