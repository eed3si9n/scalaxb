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

package scalaxb.compiler
package xsd

import scala.collection.mutable
import javax.xml.namespace.QName

import scalaxb.compiler.ConfigEntry.SymbolEncoding

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
  import ContextProcessor._

  private val logger = Log.forName("xsd.ContextProcessor")
  var config: Config
  val newline = System.getProperty("line.separator")
  val XSI_URL = "http://www.w3.org/2001/XMLSchema-instance"
  val XSI_PREFIX = "xsi"
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  val XML_PREFIX = "xml"
  val XS_URL = "http://www.w3.org/2001/XMLSchema"
  val XS_PREFIX = "xs"

  def processContext(context: XsdContext, schemas: Seq[SchemaDecl]): Unit = {
    logger.debug("processContext")

    if (config.autoPackages) config = generateAutoPackages(schemas).foldLeft(config) {case (cfg, (uri, pkg)) =>
      cfg.update(ConfigEntry.PackageNames(cfg.packageNames updated (uri, pkg)))
    }
    context.schemas ++= schemas
    context.packageNames ++= config.packageNames

    (None :: (config.packageNames.valuesIterator.toList.distinct)) map {
      pkg =>
        context.enumValueNames(pkg) = mutable.ListMap.empty[(String, EnumerationDecl[_]), String]
    }

    val anonymousTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]

    for (schema <- schemas) {
      logger.debug("processContext - " + schema.targetNamespace)
      context.typeNames(schema) =  makeProtectedTypeName(schema, context)
      resolveType(schema, context)
    }

    def registerDuplicatedType(schema: SchemaDecl, decl: Decl, name: String): Unit = {
      context.duplicatedTypes += ((schema, decl))
      logger.warn("%s is defined more than once.", name)
    }

    def nameEnumSimpleType(schema: SchemaDecl, decl: SimpleTypeDecl,
       initialName: String, postfix: String = "Type"): Unit = {
      if (context.typeNames.contains(decl)) registerDuplicatedType(schema, decl, decl.name)
      else {
        context.typeNames(decl) = makeProtectedTypeName(schema.targetNamespace, initialName, postfix, context)
        logger.debug("processContent: enum %s is named %s" format(decl.name, context.typeNames(decl)))
        makeEnumValues(decl, schema.scope, context)
      } // if-else
    }

    for {
      schema <- schemas
      elem <- schema.elemList
      typeSymbol = elem.typeSymbol
      if typeSymbol.name.contains("@")
      if typeSymbol.isInstanceOf[ReferenceTypeSymbol]
      ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol]
    } ref.decl match {
      case decl: ComplexTypeDecl =>
        anonymousTypes += ((schema, decl))
        logger.debug("processContent: %s's %s" format(elem.name, decl.name))
        if (context.typeNames.contains(decl)) registerDuplicatedType(schema, decl, elem.name)

        context.typeNames.getOrElseUpdate(decl, {
          val prefix: Option[String] =
            if (decl.family != List(elem.name, elem.name) && config.prependFamilyName) Some(decl.family.head)
            else None
          val name = makeProtectedTypeName(schema.targetNamespace, prefix, elem, context)
          logger.debug("processContent: %s's %s is named %s" format(elem.name, decl.name, name))
          name
        })

// simple types are handled later.
//      case decl: SimpleTypeDecl if containsEnumeration(decl) =>
//        logger.debug("processContent: %s's %s" format(elem.name, decl.name))
//        nameEnumSimpleType(schema, decl, elem.name, "")
      case _ =>
    }

    val namedTypes = mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)]
    for {
      schema <- schemas
      typ <- schema.topTypes
    } typ match {
      case (_, decl: ComplexTypeDecl) =>
        logger.debug("processContext: top-level type %s" format (decl.name))
        namedTypes += ((schema, decl))
        if (context.typeNames.contains(decl)) registerDuplicatedType(schema, decl, decl.name)
        else {
          val name = makeProtectedTypeName(schema.targetNamespace, decl, context)
          logger.debug("processContext: top-level type %s is named %s" format (decl.name, name))
          context.typeNames.getOrElseUpdate(decl, name)
        }
      case (_, decl@SimpleTypeDecl(_, _, _, _, _)) if containsEnumeration(decl) =>
        logger.debug("processContext: top-level type %s" format (decl.name))
        nameEnumSimpleType(schema, decl, decl.name)
      case _ =>
    }

    context.complexTypes ++= anonymousTypes.toList.distinct :::
      namedTypes.toList.distinct

    def associateSubType(subType: ComplexTypeDecl, schema: SchemaDecl, base: ComplexTypeDecl): Unit = {
      if (!context.baseToSubs.contains(base)) { context.baseToSubs(base) = Nil }

      context.baseToSubs(base) = subType :: context.baseToSubs(base)
    }

    for ((schema, typ) <- context.complexTypes)  typ.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, schema, base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        associateSubType(typ, schema, base)

      case SimpContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _, _) =>
        associateSubType(typ, schema, base)
      case SimpContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _) =>
        associateSubType(typ, schema, base)
      case _ =>
    }

    for (base <- context.baseToSubs.keysIterator.toList;
         if !base.abstractValue &&
           !context.schemas.exists(schema => context.duplicatedTypes.contains((schema, base))) ) {
      context.typeNames(base) = makeTraitName(base)
      logger.debug("processContext: naming trait %s" format context.typeNames(base))
    }

    for (schema <- schemas;
        group <- schema.topGroups.valuesIterator.toList) {
      val pair = (schema, group)
      context.groups += pair
      logger.debug("processContext: added group " + group.name)
    }

    for (schema <- schemas;
        elem <- schema.topElems.valuesIterator.toList) {
      elem.substitutionGroup foreach { sub =>
        if (!context.substituteGroups.contains(sub)) {
          context.substituteGroups += sub
          logger.debug("processContext: added sub group " + sub)
        }
      }
    }

    for {
      schema <- schemas
      typ <- schema.typeList if !(schema.topTypes.valuesIterator contains typ)
    } typ match {
      case decl: SimpleTypeDecl if containsEnumeration(decl) =>
        logger.debug("processContext: inner simple type %s" format (decl.name))
        nameEnumSimpleType(schema, decl, decl.family.last)
      case _ =>
    }

    for (schema <- schemas;
        group <- schema.topAttrGroups.valuesIterator.toList) {
      if (context.typeNames.contains(group)) registerDuplicatedType(schema, group, group.name)
      else context.typeNames(group) = makeProtectedTypeName(schema.targetNamespace, group, context)
    }

    makeCompositorNames(context)
  }

  def generateAutoPackages(schemas: Seq[SchemaDecl]): Seq[(Option[String], Option[String])] = {
    val numbers = ('0' to '9').toSet

    val allowedChars = (  // What can be in a package name
      ('a' to 'z') ++
      ('A' to 'Z')
    ).toSet ++ numbers ++ Set(
      '.'
    )

    schemas.map {s => s.targetNamespace -> s.targetNamespace.map(_
      .split("[:/]")                                           // Split the namespace URI into fragments
      .map {_.filter(allowedChars)}                            // Package name must be valid
      .filter(!_.isEmpty)                                      // Drop empty fragments
      .map {str => if (numbers(str.head)) 'n' + str else str}  // Package name can't start with a number
      .tail                                                    // Drop the first fragment, which is usually "http"
      .mkString(".")                                           // Concat the fragments
    )}
  }

  def getTypeGlobally(namespace: Option[String], typeName: String, context: XsdContext): TypeDecl =
    (for (schema <- context.schemas;
        if schema.targetNamespace == namespace;
        if schema.topTypes.contains(typeName))
      yield schema.topTypes(typeName)).headOption getOrElse {
        throw new ReferenceNotFound("type" , namespace, typeName)
      }

  def resolveType(schema: SchemaDecl, context: XsdContext) : Unit = {
    def containsTypeLocally(namespace: Option[String], typeName: String): Boolean =
      (namespace == schema.targetNamespace && schema.topTypes.contains(typeName))

    // try to resolve the symbol using local schema first
    def resolveTypeSymbol(typeSymbol: XsTypeSymbol) : Unit = {
      typeSymbol match {
        case symbol: ReferenceTypeSymbol =>
          if (symbol.decl != null) ()
          else {
            if (containsTypeLocally(symbol.namespace, symbol.localPart)) symbol.decl = schema.topTypes(symbol.localPart)
            else symbol.decl = getTypeGlobally(symbol.namespace, symbol.localPart, context)
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

  lazy val enumNameMaxLength = config.enumNameMaxLength

  def makeEnumValues(decl: SimpleTypeDecl, scope: scala.xml.NamespaceBinding, context: XsdContext) : Unit = {
    val enumValues = context.enumValueNames(packageName(decl.namespace, context))
    val name = context.typeNames(decl)
    filterEnumeration(decl) map { enum =>
      enumValues(name -> enum) = makeProtectedTypeName(decl.namespace,
        enum.value match {
          case qname: QName => Option(scope.getPrefix(qname.getNamespaceURI)).getOrElse("") + qname.getLocalPart.capitalize
          case x if enum.value.toString.length > enumNameMaxLength => "longName"
          case _            => enum.value.toString
        }, "Value", context)
    }
  }

  def containsEnumeration(decl: SimpleTypeDecl) = decl.content match {
    case x: SimpTypRestrictionDecl =>
      x.facets exists { f => f match {
          case e: EnumerationDecl[_] => true
          case _ => false
        }
      }

    case _ => false
  }

  def filterEnumeration(decl: SimpleTypeDecl): List[EnumerationDecl[_]] = decl.content match {
    case x: SimpTypRestrictionDecl =>
      x.facets collect {
        case e: EnumerationDecl[_] => e
      }

    case _ => Nil
  }

  def makeGroupComplexType(group: GroupDecl) =
    ComplexTypeDecl(group.namespace, group.name, List(group.name), false, false,
      ComplexContentDecl.empty, Nil, None)

  def containsSingleChoice(seq: SequenceDecl) = seq.particles match {
    case ChoiceDecl(_, _, _, _, _) :: Nil => true
    case _ => false
  }

  def singleChoice(seq: SequenceDecl): ChoiceDecl = seq.particles match {
    case (choice@ChoiceDecl(_, _, _, _, _)) :: Nil => choice
    case _ => sys.error("Does not cointain single choice.")
  }

  lazy val sequenceChunkSize = config.sequenceChunkSize
  lazy val contentsSizeLimit = config.contentsSizeLimit
  lazy val namedAttributes = config.namedAttributes

  def isWrapped(decl: ComplexTypeDecl): Boolean = isWrapped(decl.namespace, decl.family)
  def isWrapped(namespace: Option[String], family: List[String]): Boolean =
    (namespace map { ns =>
      config.wrappedComplexTypes.contains("{" + ns + "}" + family.head) } getOrElse { false }) ||
    config.wrappedComplexTypes.contains(family.head)

  def splitLong[A <: HasParticle](rest: List[Particle])(f: (List[Particle]) => A): List[A] =
    if (rest.size <= sequenceChunkSize) List(f(rest))
    else List(f(rest.take(sequenceChunkSize))) ::: splitLong[A](rest.drop(sequenceChunkSize))(f)

  def makeCompositorNames(context: XsdContext): Unit = {
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
        case p => sys.error("ContextProcessor#makeCompositorNames: unexpected particle type: " + p.getClass.getName)
      }
      else sys.error("ContextProcessor#makeCompositorNames: group must contain one content model: " + group)
    }

    def isFirstCompositor =
      (sequenceNumber + choiceNumber + allNumber == 0)

    def makeGroupCompositorName(compositor: HasParticle, group: GroupDecl): Unit = {
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
            splitLong[SequenceDecl](seq.particles) { formSequence(makeGroupComplexType(group), group.name, _) }
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

    def formSequence(decl: ComplexTypeDecl, baseName: String, rest: List[Particle]) = {
      val retval = SequenceDecl(decl.namespace, rest, 1, 1, 0)
      context.compositorNames(retval) = baseName + "Sequence" + apparentSequenceNumber
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

    def makeCompositorName(compositor: HasParticle, decl: ComplexTypeDecl): Unit = {
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
            splitLong[SequenceDecl](seq.particles) { formSequence(decl, familyName(decl), _) }
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
      val l = value.toLowerCase
      val enumValueNames = context.enumValueNames(packageName(namespace, context))
      (context.typeNames exists {
        case (k: NameKey, v: String) =>
          packageName (k.namespace, context) == packageName(namespace, context) &&
          v.toLowerCase == l
      }) ||
      (enumValueNames.valuesIterator exists { x => x.toLowerCase == l })
    }

    var name = makeTypeName(initialName)
    if (!contains(name)) name
    else {
      name = makeTypeName(initialName) + postfix
      var i = 2
      while (contains(name)) {
        name = makeTypeName(initialName) + postfix + i
        i = i + 1
      }
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
        if (p.endsWith("_"))  p.capitalize + identifier(name)
        else p.capitalize + identifier(name).capitalize
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

  def makeParamName(name0: String, attribute: Boolean) = {
    val prefix = (config.paramPrefix, config.attributePrefix) match {
      case (p, _) if !attribute => p
      case (None, None) => None
      case (p, a) => Some(p.getOrElse("") + a.getOrElse(""))
    }

    val name = name0.trim
    val base = prefix map { p =>
      if (p.endsWith("_"))  p + name
      else p + name.capitalize
    } getOrElse { name }

    if (isKeyword(base) || isCommonlyUsedWord(base)) identifier(base + "Value")
    else if (attribute && isSpecialAttributeWord(base)) identifier(base + "Attribute")
    else if (startsWithNumber(base)) identifier("number" + base)
    else identifier(base)
  }

  def makePrefix(namespace: Option[String], context: XsdContext): String = namespace map { ns =>
    if (ns == XML_URI) XML_PREFIX
    else context.prefixes.getOrElse(ns, "")
  } getOrElse {""}

  private lazy val symbolEncoder = SymbolEncoder(config.symbolEncodingStrategy)

  def identifier(value: String) = {
    def normalize(c: Char): String = {
      val encoded = symbolEncoder(c)
      if (config.capitalizeWords) encoded.capitalize else encoded
    }

    def normalizeUnless(isAcceptable: Char => Boolean, str: Iterable[Char]): Iterable[String] = str.map { c =>
      if (isAcceptable(c)) c.toString else normalize(c)
    }

    def toCamelCase(str: String) = {
      if (config.capitalizeWords) ContextProcessor.toCamelCase(str) else str
    }

    // treat "" as "blank" but " " as "u32"
    val nonspace =
      if (value.trim != "") """\s""".r.replaceAllIn(toCamelCase(value), "")
      else value

    import Character._
    val numbers = ('0' to '9').toSet
    def isScalaxbIdentifierStart(value: Char): Boolean = isJavaIdentifierStart(value) || numbers.contains(value)
    val validfirstchar: String = nonspace.headOption.toIterable.flatMap { firstChar =>
      normalizeUnless(isScalaxbIdentifierStart, Seq(firstChar)) ++ normalizeUnless(isJavaIdentifierPart, nonspace.tail)
    }.mkString

    // Scala identifiers must not end with an underscore
    // Known issue: if `discardNonIdentifierCharacters` is set and an identifier ends in multiple underscores (e.g. `el__`)
    //              then the generated name will be invalid (`el_`, as only the last underscore will be dropped)
    if (validfirstchar.endsWith("_")) validfirstchar.dropRight(1) + normalize(validfirstchar.last)
    else if (validfirstchar == "") "blank"
    else validfirstchar
  }

  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }

  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""

  def indent(indent: Int) = "  " * indent
}

object ContextProcessor {
  type SymbolEncoder = Char => String
  object SymbolEncoder {
    import SymbolEncoding._
    def apply(strategy: SymbolEncoding.Strategy): SymbolEncoder = strategy match {
      case Discard      => _ => ""
      case SymbolName   => symbolName
      case UnicodePoint => unicodePoint
      case DecimalAscii => decimalAscii
      case Legacy151    => legacy151
    }

    private def symbolName(c: Char) = SpecialCharacterNames.getOrElse(c, unicodePoint(c))
    private def unicodePoint(c: Char) = f"u${c.toInt}%04x"
    private def decimalAscii(c: Char) = s"u${c.toInt}"
    /** In v1.5.1, trailing underscores were encoded as "u93", even though the ASCII code for underscore is 95 */
    private def legacy151(c: Char) = if (c == '_') "u93" else decimalAscii(c)
  }

  /** Names of the symbolic characters acceptable in an XML Name, according to the spec:
    * https://www.w3.org/TR/xml/#NT-NameStartChar
    */
  private val SpecialCharacterNames = Map(
    '-' -> "hyphen",
    '.' -> "dot",
    ':' -> "colon",
    '_' -> "underscore"
  )

  private def toCamelCase(value: String): String = {
    val words = value.split(raw"\b")  // word boundary
    (words.head +: words.tail.map(_.capitalize)).mkString
  }
}
