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

import scalaxb.compiler.{ScalaNames, Logger, Config, ReferenceNotFound}
import scala.collection.mutable

trait PackageName {
  def packageName(schema: SchemaDecl, context: XsdContext): Option[String] =
    packageName(schema.targetNamespace, context)

  def packageName(decl: ComplexTypeDecl, context: XsdContext): Option[String] =
    packageName(decl.namespace, context)
  
  def packageName(decl: SimpleTypeDecl, context: XsdContext): Option[String] =
    packageName(decl.namespace, context)

  def packageName(group: AttributeGroupDecl, context: XsdContext): Option[String] =
    packageName(group.namespace, context)
      
  def packageName(namespace: Option[String], context: XsdContext): Option[String] =
    if (context.packageNames.contains(namespace)) context.packageNames(namespace)
    else if (context.packageNames.contains(None)) context.packageNames(None)
    else None
}

trait ContextProcessor extends ScalaNames with PackageName {
  def logger: Logger
  def log(msg: String) = logger.log(msg)
  def config: Config
  val newline = System.getProperty("line.separator")
  val XSI_URL = "http://www.w3.org/2001/XMLSchema-instance"
  val XSI_PREFIX = "xsi"
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val XML_PREFIX = "xml"
  val XS_URL = "http://www.w3.org/2001/XMLSchema"
  val XS_PREFIX = "xs"
  
  def processContext(context: XsdContext) {
    log("xsd.ContextProcessor#processContext")
    context.packageNames ++= config.packageNames
    
    (None :: (config.packageNames.valuesIterator.toList.distinct)) map {
      pkg =>
        context.enumValueNames(pkg) = mutable.ListMap.empty[(String, EnumerationDecl), String]
    }
    
    val anonymousTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]
    
    for (schema <- context.schemas) {
      context.typeNames(schema) =  makeProtectedTypeName(schema, context)
      resolveType(schema, context)
    }
    
    def nameEnumSimpleType(schema: SchemaDecl, decl: SimpleTypeDecl,
       initialName: String, postfix: String = "Type") {
      if (!context.typeNames.contains(decl)) {
        context.typeNames(decl) = makeProtectedTypeName(schema.targetNamespace, initialName, postfix, context)
        makeEnumValues(decl, context)
      } // if
    }

    def nameElemComplexType(schema: SchemaDecl, elem: ElemDecl, decl: ComplexTypeDecl): String = {
      context.typeNames.getOrElseUpdate(decl, {
        val prefix: Option[String] =
          if (decl.family != List(elem.name, elem.name) && config.prependFamilyName) Some(decl.family.head)
          else None
        val name = makeProtectedTypeName(schema.targetNamespace, prefix, elem, context)
        log("ContextProcessor#nameElemComplexType: %s's %s is named %s" format(elem.name, decl.name, name))
        name
      })
    }

    for {
      schema <- context.schemas
      elem <- schema.elemList
      val typeSymbol = elem.typeSymbol
      if typeSymbol.name.contains("@")
      if typeSymbol.isInstanceOf[ReferenceTypeSymbol]
      val ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol]
    } ref.decl match {
      case decl: ComplexTypeDecl =>          
        val pair = (schema, decl)
        anonymousTypes += pair
        nameElemComplexType(schema, elem, decl)
      case decl: SimpleTypeDecl if containsEnumeration(decl) =>
        nameEnumSimpleType(schema, decl, elem.name, "")
      case _ =>
    }
    
    val namedTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]
        
    for (schema <- context.schemas;
        typ <- schema.topTypes) typ match {
      case (_, decl: ComplexTypeDecl) =>   
        val pair = (schema, decl)
        namedTypes += pair
        context.typeNames.getOrElseUpdate(decl, makeProtectedTypeName(schema.targetNamespace, decl, context))
      case (_, decl@SimpleTypeDecl(_, _, _, _, _)) if containsEnumeration(decl) =>
        nameEnumSimpleType(schema, decl, decl.name)
      case _ =>      
    }
    
    context.complexTypes ++= anonymousTypes.toList.distinct :::
      namedTypes.toList.distinct
      
    for (schema <- context.schemas;
        group <- schema.topGroups.valuesIterator.toList) {
      val pair = (schema, group)
      context.groups += pair
      log("ContextProcessor#processContext: added group " + group.name) 
    }
    
    for (schema <- context.schemas;
        elem <- schema.topElems.valuesIterator.toList) {
      elem.substitutionGroup foreach { sub =>
        if (!context.substituteGroups.contains(sub)) {
          context.substituteGroups += sub
          log("ContextProcessor#processContext: added sub group " + sub) 
        }
      }
    }
    
    for (schema <- context.schemas;
        typ <- schema.typeList) typ match {
      case decl: SimpleTypeDecl if containsEnumeration(decl) => nameEnumSimpleType(schema, decl, decl.family.head)
      case _ =>      
    }
    
    for (schema <- context.schemas;
        group <- schema.topAttrGroups.valuesIterator.toList) {
      context.typeNames.getOrElseUpdate(group, makeProtectedTypeName(schema.targetNamespace, group, context))
    }
    
    def associateSubType(subType: ComplexTypeDecl, base: ComplexTypeDecl) {
      if (context.baseToSubs.contains(base)) context.baseToSubs(base) = subType :: context.baseToSubs(base)
      else context.baseToSubs(base) = subType :: Nil
    }
    
    for ((schema, typ) <- context.complexTypes)  typ.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, base)
      
      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) =>
        associateSubType(typ, base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, base)
      case _ =>
    }
    
    for (base <- context.baseToSubs.keysIterator;
        if !base.abstractValue) {
      context.typeNames.getOrElseUpdate(base, makeTraitName(base))
    }
    
    makeCompositorNames(context)
  }

  def getTypeGlobally(namespace: Option[String], typeName: String, context: XsdContext): TypeDecl =
    (for (schema <- context.schemas;
        if schema.targetNamespace == namespace;
        if schema.topTypes.contains(typeName))
      yield schema.topTypes(typeName)).headOption getOrElse {
        throw new ReferenceNotFound("type" , namespace, typeName)
      }

  def resolveType(schema: SchemaDecl, context: XsdContext) {
    def containsTypeLocally(namespace: Option[String], typeName: String): Boolean =
      (namespace == schema.targetNamespace && schema.topTypes.contains(typeName))

    // try to resolve the symbol using local schema first
    def resolveTypeSymbol(typeSymbol: XsTypeSymbol) {
      typeSymbol match {
        case symbol: ReferenceTypeSymbol =>
          if (symbol.decl != null) symbol.decl
          else {
            val (namespace, typeName) = TypeSymbolParser.splitTypeName(symbol.name, schema)
            if (containsTypeLocally(namespace, typeName)) symbol.decl = schema.topTypes(typeName)
            else symbol.decl = getTypeGlobally(namespace, typeName, context)
          }
        case _ =>
      }
    }

    for (elem <- schema.elemList) resolveTypeSymbol(elem.typeSymbol)
    
    for (attr <- schema.attrList) attr.typeSymbol match {
      case symbol: ReferenceTypeSymbol => resolveTypeSymbol(symbol)
      case _ =>
    } // match    
    
    for (typ <- schema.typeList) typ match {
      case SimpleTypeDecl(_, _, _, res: SimpTypRestrictionDecl, _) =>
        resolveTypeSymbol(res.base)
      case SimpleTypeDecl(_, _, _, list: SimpTypListDecl, _) =>
        resolveTypeSymbol(list.itemType) 
      case ComplexTypeDecl(_, _, _, _, _, SimpleContentDecl(res: SimpContRestrictionDecl), _, _) =>
        resolveTypeSymbol(res.base)
      case ComplexTypeDecl(_, _, _, _, _, SimpleContentDecl(ext: SimpContExtensionDecl), _, _) =>
        resolveTypeSymbol(ext.base)      
      case ComplexTypeDecl(_, _, _, _, _, ComplexContentDecl(res: CompContRestrictionDecl), _, _) =>
        resolveTypeSymbol(res.base)
      case ComplexTypeDecl(_, _, _, _, _, ComplexContentDecl(ext: CompContExtensionDecl), _, _) =>
        resolveTypeSymbol(ext.base)  
      case _ =>
    }
  }
  
  def makeEnumValues(decl: SimpleTypeDecl, context: XsdContext) {
    val enumValues = context.enumValueNames(packageName(decl.namespace, context))
    val name = context.typeNames(decl)
    filterEnumeration(decl) map { enum =>
      enumValues(name -> enum) = makeProtectedTypeName(decl.namespace, enum.value, "Value", context)
    }
  }
  
  def containsEnumeration(decl: SimpleTypeDecl) = decl.content match {
    case x: SimpTypRestrictionDecl =>
      x.facets exists { f => f match {
          case e: EnumerationDecl => true
          case _ => false
        }
      }
    
    case _ => false
  }
  
  def filterEnumeration(decl: SimpleTypeDecl): List[EnumerationDecl] = decl.content match {
    case x: SimpTypRestrictionDecl =>
      x.facets collect {
        case e: EnumerationDecl => e
      }
    
    case _ => Nil
  }
  
  def makeGroupComplexType(group: GroupDecl) =
    ComplexTypeDecl(group.namespace, group.name, List(group.name), false, false,
      ComplexContentDecl.empty, Nil, None)

  def containsSingleChoice(seq: SequenceDecl) = seq.particles match {
    case ChoiceDecl(_, _, _, _) :: Nil => true
    case _ => false
  }
  
  def singleChoice(seq: SequenceDecl): ChoiceDecl = seq.particles match {
    case (choice@ChoiceDecl(_, _, _, _)) :: Nil => choice
    case _ => error("Does not cointain single choice.")
  }
  
  lazy val sequenceChunkSize = config.sequenceChunkSize
  lazy val contentsSizeLimit = config.contentsSizeLimit

  def isWrapped(decl: ComplexTypeDecl): Boolean = isWrapped(decl.namespace, decl.family)
  def isWrapped(namespace: Option[String], family: List[String]): Boolean =
    (namespace map { ns =>
      config.wrappedComplexTypes.contains("{" + ns + "}" + family.head) } getOrElse { false }) ||
    config.wrappedComplexTypes.contains(family.head)
  
  def splitLong[A <: HasParticle](rest: List[Particle])(f: (List[Particle]) => A): List[A] =
    if (rest.size <= sequenceChunkSize) List(f(rest))
    else List(f(rest.take(sequenceChunkSize))) ::: splitLong[A](rest.drop(sequenceChunkSize))(f)
    
  def makeCompositorNames(context: XsdContext) {
    var sequenceNumber = 0
    var choiceNumber = 0
    var allNumber = 0
    var isFirstCompositorSequence = false
    
    for ((schema, decl) <- context.complexTypes) { 
      sequenceNumber = 0
      choiceNumber = 0
      allNumber = 0
      isFirstCompositorSequence = false
      
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
      isFirstCompositorSequence = false
      
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
        case seq: SequenceDecl =>
          if (!isFirstCompositor ||
              !containsSingleChoice(seq))
            context.compositorParents(compositor) = makeGroupComplexType(group)
          
          if (isFirstCompositor) context.compositorNames(compositor) = groupName + "Sequence"
          else context.compositorNames(compositor) = groupName + "Sequence" + apparentSequenceNumber
          sequenceNumber += 1
          
          if (seq.particles.size > contentsSizeLimit || isWrapped(group.namespace, List(group.name)))
            splitLong[SequenceDecl](seq.particles) { formSequence(makeGroupComplexType(group), _) }
        case choice: ChoiceDecl =>
          context.compositorParents(compositor) = makeGroupComplexType(group)
          if (isFirstCompositor) context.compositorNames(compositor) = groupName + "Option"
          else context.compositorNames(compositor) = groupName + "Option" + (choiceNumber + 1)
          choiceNumber += 1
          
        case all: AllDecl =>
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
    
    def formSequence(decl: ComplexTypeDecl, rest: List[Particle]) = {      
      val retval = SequenceDecl(rest, 1, 1, 0)
      context.compositorNames(retval) = familyName(decl) + "Sequence" + apparentSequenceNumber
      sequenceNumber += 1
      context.compositorParents(retval) = decl
      retval
    }
    
    def apparentSequenceNumber = if (isFirstCompositorSequence) sequenceNumber else sequenceNumber + 1

    def familyName(decl: ComplexTypeDecl): String = {
      val x = context.typeNames(decl)
      val prefixed = config.classPrefix map { p => x.drop(p.length) } getOrElse {x}
      config.classPostfix map { p => prefixed.dropRight(p.length) } getOrElse {prefixed}
    }

    def makeCompositorName(compositor: HasParticle, decl: ComplexTypeDecl) {
      compositor match {
        case seq: SequenceDecl =>
          val separateSequence = if (!isFirstCompositor ||
              seq.minOccurs != 1 || seq.maxOccurs != 1)
            if (seq.particles.size == 1) seq.particles(0) match {
              case any: AnyDecl => false
              case choice: ChoiceDecl => false
              case _ => true
            } 
            else true
          else false
          
          if (separateSequence) {
            context.compositorParents(compositor) = decl
            context.compositorNames.getOrElseUpdate(compositor, familyName(decl) + "Sequence" + apparentSequenceNumber)
          }
          else {
            isFirstCompositorSequence = true
            context.compositorNames.getOrElseUpdate(compositor, familyName(decl))
          }
          
          sequenceNumber += 1
          
          if (seq.particles.size > contentsSizeLimit || isWrapped(decl.namespace, decl.family))
            splitLong[SequenceDecl](seq.particles) { formSequence(decl, _) }
        case choice: ChoiceDecl =>
          context.compositorParents(compositor) = decl
          if (choiceNumber == 0) context.compositorNames.getOrElseUpdate(compositor, familyName(decl) + "Option")
          else context.compositorNames.getOrElseUpdate(compositor, familyName(decl) + "Option" + (choiceNumber + 1))
          choiceNumber += 1
          
        case all: AllDecl =>
          context.compositorParents(compositor) = decl
          if (allNumber == 0) context.compositorNames.getOrElseUpdate(compositor, familyName(decl) + "All")
          else context.compositorNames.getOrElseUpdate(compositor, familyName(decl) + "All" + (allNumber + 1))
          allNumber += 1
        case _ =>
      }
      
      compositor.particles collect {
        case compositor2: HasParticle => makeCompositorName(compositor2, decl)
      }
    } // makeCompositorName
  }
  
  def makeProtectedTypeName(namespace: Option[String], initialName: String, postfix: String,
      context: XsdContext): String = {
    def contains(value: String) = {
      val enumValueNames = context.enumValueNames(packageName(namespace, context))
      (context.typeNames exists {
        case (k: NameKey, v: String) =>
          packageName (k.namespace, context) == packageName(namespace, context) && v == value
      }) ||
      enumValueNames.valuesIterator.contains(value)
    }
    
    var name = makeTypeName(initialName)
    if (!contains(name)) name
    else {
      name = makeTypeName(initialName) + postfix
      for (i <- 2 to 100) {
        if (contains(name)) name = makeTypeName(initialName) + postfix + i
      } // for i
      name
    }    
  }
  
  def makeProtectedTypeName(schema: SchemaDecl, context: XsdContext): String =
    makeProtectedTypeName(schema.targetNamespace, "XMLProtocol", "", context)
  
  def makeProtectedTypeName(namespace: Option[String], prefix: Option[String],
                            elem: ElemDecl, context: XsdContext): String =
    makeProtectedTypeName(elem.namespace orElse namespace,
      prefix map { "%s%s" format (_, elem.name.capitalize) } getOrElse {elem.name}, "", context)
  
  def makeProtectedTypeName(namespace: Option[String], decl: ComplexTypeDecl, context: XsdContext): String =
    makeProtectedTypeName(decl.namespace orElse namespace, decl.name, "Type", context)
    
  def makeProtectedTypeName(namespace: Option[String], decl: SimpleTypeDecl, context: XsdContext): String =
    makeProtectedTypeName(decl.namespace orElse namespace, decl.name, "Type", context)
  
  def makeProtectedTypeName(namespace: Option[String], attr: AttributeDecl, context: XsdContext): String =
    makeProtectedTypeName(attr.namespace orElse namespace, attr.name, "Type", context)

  def makeProtectedTypeName(namespace: Option[String], group: AttributeGroupDecl, context: XsdContext): String =
    makeProtectedTypeName(group.namespace orElse namespace, group.name, "Type", context)
      
  def makeTraitName(decl: ComplexTypeDecl) =
    if (decl.name.last == 'e')
      makeTypeName(decl.name.dropRight(1) + "able")
    else makeTypeName(decl.name + "able")
  
  def makeTypeName(name: String) = name match {
    case s if (s.startsWith("java.") || s.startsWith("javax.")) => s
    case _ =>
      val prefixed = config.classPrefix map { p =>
        if (p.endsWith("_"))  p.capitalize + name
        else p.capitalize + name.capitalize
      } getOrElse { identifier(name).capitalize }
      val base = config.classPostfix map { p =>
        prefixed + p
      } getOrElse {prefixed}

      if (startsWithNumber(base)) "Number" + base
      else if (isCommonlyUsedWord(base)) base + "Type"
      else base
  }
  
  def startsWithNumber(name: String) =
    """\d""".r.findPrefixMatchOf(name) match {
      case Some(_) => true
      case _ => false
    }
  
  def makeParamName(name: String) = {
    val base = config.paramPrefix map { p =>
      if (p.endsWith("_"))  p + name
      else p + name.capitalize
    } getOrElse { name }
    
    if (isKeyword(base)) identifier(base + "Value")
    else if (startsWithNumber(base)) identifier("number" + base)
    else identifier(base)
  }
  
  def makePrefix(namespace: Option[String], context: XsdContext): String = namespace map { ns =>
    if (ns == XML_URI) XML_PREFIX
    else context.prefixes.getOrElse(ns, "")
  } getOrElse {""}
      
  def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")
      
  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }
  
  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""

  def indent(indent: Int) = "  " * indent
}
