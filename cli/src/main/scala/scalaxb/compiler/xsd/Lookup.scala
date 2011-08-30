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

  def elements(qname: (Option[String], String)): ElemDecl = elements(qname._1, qname._2)

  def elements(namespace: Option[String], name: String) =
    (for (schema <- schemas;
          if schema.targetNamespace == namespace;
          if schema.topElems.contains(name))
        yield schema.topElems(name)) match {
        case x :: xs => x
        case Nil     =>
          (namespace, name) match {
            case (Some(XS_URL), "schema") => ElemDecl(namespace, name, XsAnyType, None, None, 1, 1)
            case _ => throw new ReferenceNotFound("element" , namespace, name)
          }
      }  

  // http://www.w3.org/TR/xmlschema-0/#Globals
  // In other words, global declarations cannot contain the attributes
  // minOccurs, maxOccurs, or use.
  def buildElement(ref: ElemRef) =
    Some(elements(ref.namespace, ref.name)) map { that =>
      that.copy(minOccurs = ref.minOccurs,
        maxOccurs = ref.maxOccurs,
        nillable = ref.nillable match {
          case None => that.nillable
          case _    => ref.nillable
        })
    } get

  def buildSymbolElement(symbol: XsTypeSymbol): ElemDecl =
    ElemDecl(schema.targetNamespace, "value", symbol, None, None, 1, 1)

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
    ("lang" -> AttributeDecl(Some(XML_URI), "lang", XsString)),
    ("space" -> AttributeDecl(Some(XML_URI), "space", XsString)),
    ("base" -> AttributeDecl(Some(XML_URI), "base", XsAnyURI)),
    ("id" -> AttributeDecl(Some(XML_URI), "id", XsID))
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

  // http://www.w3.org/TR/xmlschema-0/#Globals
  // In other words, global declarations cannot contain the attributes
  // minOccurs, maxOccurs, or use.
  def buildAttribute(ref: AttributeRef) =
    Some(attrs(ref.namespace, ref.name)) map { that =>
      that.copy(defaultValue = ref.defaultValue,
        fixedValue = ref.fixedValue,
        use = ref.use)
    } get

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
    case AnyType(symbol) => "scalaxb.DataRecord[Any]"
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
    if (!context.typeNames.contains(decl)) error(pkg + ": Type name not found: " + decl.toString)
    
    if (shortLocal && pkg == packageName(schema, context)) context.typeNames(decl)
    else buildFullyQualifiedName(pkg, context.typeNames(decl))
  }
  
  def buildTypeName(decl: SimpleTypeDecl, shortLocal: Boolean): String = decl.content match {
    case x@SimpTypRestrictionDecl(_, _) if containsEnumeration(decl)  => buildEnumTypeName(decl, shortLocal)
    case x: SimpTypRestrictionDecl                                    =>
      buildTypeName(baseType(decl), shortLocal)
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

  def baseType(decl: SimpleTypeDecl): XsTypeSymbol = decl.content match {
    case SimpTypRestrictionDecl(base, _) => base
    case SimpTypListDecl(itemType) => itemType
    case SimpTypUnionDecl() => XsString

    case _ => error("GenSource: Unsupported content " +  decl.content.toString)
  }
  
  def containsForeignType(compositor: HasParticle): Boolean = {
    def elemContainsForeignType(elem: ElemDecl): Boolean = elem.typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) => packageName(decl.namespace, context) != packageName(schema, context)
      case ReferenceTypeSymbol(decl: SimpleTypeDecl)  => packageName(decl.namespace, context) != packageName(schema, context)
      case _ => true
    }

    compositor.particles.exists(_ match {
        case elem: ElemDecl => elemContainsForeignType(elem)
        case ref: ElemRef   => elemContainsForeignType(buildElement(ref))
        case ref: GroupRef  => ref.namespace != schema.targetNamespace
        case _ => false
      }
    )
  }

  def isSubstitionGroup(elem: ElemDecl) =
    elem.global && (elem.namespace map { x =>
      context.substituteGroups.contains((elem.namespace, elem.name))
    } getOrElse { false })
  
  def quoteNamespace(namespace: Option[String]): String =
    if (namespace == schema.targetNamespace) "targetNamespace"
    else quote(namespace)

  def elementNamespace(global: Boolean, namespace: Option[String], qualified: Boolean): Option[String] =
    if (global) namespace
    else if (qualified) schema.targetNamespace
    else None

  def elementNamespaceString(global: Boolean, namespace: Option[String], qualified: Boolean): String =
    quoteNamespace(elementNamespace(global, namespace, qualified))
}
 