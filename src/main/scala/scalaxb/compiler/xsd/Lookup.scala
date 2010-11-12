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

import scalaxb.compiler.{Logger}
import scala.collection.mutable

trait Lookup extends ContextProcessor {
  def logger: Logger
  def schema: SchemaDecl
  def context: XsdContext
  
  val schemas = context.schemas.toList
  val newline = System.getProperty("line.separator")
  val compositorWrapper = mutable.ListMap.empty[ComplexTypeDecl, HasParticle]
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val INTERNAL_NAMESPACE = "http://scalaxb.org/internal"
  
  def elements(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     => error("Element not found: {" + namespace + "}" + name)
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
       case Nil     => error("Group not found: {" + namespace + "}" + name)
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
          case Nil     => error("Attribute not found: {" + namespace + "}:" + name)
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
        case Nil     => error("Attribute group not found: {" + namespace + "}" + name)
      }
      
  def buildAttributeGroup(ref: AttributeGroupRef) =
    attributeGroups(ref.namespace, ref.name)
  
  def buildTypeName(typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case XsAny          => "scalaxb.DataRecord[Any]"
    case XsNillableAny  => "scalaxb.DataRecord[Option[Any]]"
    case XsDataRecord(ReferenceTypeSymbol(decl: ComplexTypeDecl)) if compositorWrapper.contains(decl) =>
      compositorWrapper(decl) match {
        case choice: ChoiceDecl => buildChoiceTypeName(decl, choice)
        case _ => "scalaxb.DataRecord[Any]"
      }
    case r: XsDataRecord => "scalaxb.DataRecord[Any]"
    case XsMixed        => "scalaxb.DataRecord[Any]"
    case XsAnyAttribute => "scalaxb.DataRecord[String]"
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl)
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl)
    case symbol: AttributeGroupSymbol => buildTypeName(attributeGroups(symbol.namespace, symbol.name))
    case XsXMLFormat(decl: ComplexTypeDecl) => "scalaxb.XMLFormat[" + buildTypeName(decl) + "]"
    case XsXMLFormat(group: AttributeGroupDecl) => "scalaxb.XMLFormat[" + buildTypeName(group) + "]"
  }
  
  def buildChoiceTypeName(decl: ComplexTypeDecl, choice: ChoiceDecl): String
  
  def xmlFormatTypeName(decl: ComplexTypeDecl): String =
    "scalaxb.XMLFormat[" + buildTypeName(decl) + "]"
  
  def buildTypeName(decl: ComplexTypeDecl, localOnly: Boolean = false): String = {
    val pkg = packageName(decl, context)
    val typeNames = context.typeNames(pkg)
    if (!typeNames.contains(decl))
      error(pkg + ": Type name not found: " + decl.toString)
    
    if (localOnly) typeNames(decl)
    else if (pkg == packageName(schema, context)) typeNames(decl)
    else pkg match {
      case Some(x) => x + "." + typeNames(decl)
      case None => typeNames(decl)
    }
  }
  
  def buildEnumTypeName(decl: SimpleTypeDecl): String =
    buildTypeName(packageName(decl, context), decl)
  
  def buildTypeName(pkg: Option[String], decl: Decl): String = {
    val typeNames = context.typeNames(pkg)
    if (!typeNames.contains(decl))
      error(pkg + ": Type name not found: " + decl.toString)
    
    if (pkg == packageName(schema, context)) typeNames(decl)
    else pkg match {
      case Some(x) => x + "." + typeNames(decl)
      case None => typeNames(decl)
    }    
  }
  
  def buildTypeName(decl: SimpleTypeDecl): String = decl.content match {
    case x@SimpTypRestrictionDecl(_, _) if containsEnumeration(decl)  => buildEnumTypeName(decl)
    case x: SimpTypRestrictionDecl                                    => buildTypeName(baseType(decl))
    case SimpTypListDecl(ReferenceTypeSymbol(itemType: SimpleTypeDecl)) if containsEnumeration(itemType) =>
      "Seq[" + buildEnumTypeName(itemType) + "]"
    case x: SimpTypListDecl => "Seq[" + buildTypeName(baseType(decl)) + "]"
    case x: SimpTypUnionDecl => buildTypeName(baseType(decl))
  }
  
  def buildTypeName(group: AttributeGroupDecl): String =
    buildTypeName(packageName(group, context), group)
  
  def buildTypeName(enumTypeName: String, enum: EnumerationDecl): String = {
    val pkg = packageName(schema, context)
    val typeNames = context.enumValueNames(pkg)
    if (!typeNames.contains(enumTypeName, enum))
      error(pkg + ": Type name not found: " + enum.toString)
    
    if (pkg == packageName(schema, context)) typeNames(enumTypeName, enum)
    else pkg match {
      case Some(x) => x + "." + typeNames(enumTypeName, enum)
      case None => typeNames(enumTypeName, enum)
    }    
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
    
  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }

  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""
  
  def indent(indent: Int) = "  " * indent
}
 