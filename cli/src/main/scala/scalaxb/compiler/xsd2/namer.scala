package scalaxb.compiler.xsd2

import scalashim._
import java.net.URI
import xmlschema._
import scalaxb.compiler.{ScalaNames, Log}
import Defs._
import treehugger.forest._
import definitions._
import treehuggerDSL._

trait PackageNamer {
  def packageNameByURI(namespace: Option[URI], context: SchemaContext): Option[String] =
    packageName(namespace map {_.toString}, context)

  def packageName(namespace: Option[String], context: SchemaContext): Option[String] =
    if (context.packageNames.contains(namespace)) context.packageNames(namespace)
    else if (context.packageNames.contains(None)) context.packageNames(None)
    else None
}

trait Namer extends ScalaNames { self: Lookup with Splitter  =>
  val ATTRS_PARAM = "attributes"
  val ALL_PARAM = "all"
  val MIXED_PARAM = "mixed"

  private val logger = Log.forName("xsd2.Namer")

  private def names = context.names
  private def traitNames = context.traitNames
  
  def nameElementTypes(tagged: Tagged[XElement]) {
    tagged.localType map { _ match {
      case x: TaggedComplexType =>
        val name = makeProtectedElementTypeName(tagged)
        names(x) = name
        logger.debug("nameElementTypes: named %s", name)
        nameComplexTypeCompositors(x)
      case x: TaggedSimpleType =>
        val name = makeProtectedElementTypeName(tagged)
        logger.debug("nameElementTypes: named %s", name)
        names(x) = name
        nameEnumValues(x, scopeEv)
      case _ =>
    }}
  }

  def nameSimpleTypes(decl: TaggedType[XSimpleType]) {
    if (containsEnumeration(decl)) {
      val name = makeProtectedSimpleTypeName(decl)
      logger.debug("nameSimpleTypes: named %s", name)
      names(decl) = name
      nameEnumValues(decl, scopeEv)
    }
  }

  def nameTrait(decl: TaggedType[XComplexType]) {
    val rawName = decl.name.get
    val initialName = if (rawName.last == 'e') rawName.dropRight(1) + "able"
                      else rawName + "able"
    val name = makeProtectedTypeName(initialName, "Type", decl.tag, true)
    logger.debug("nameTrait: named %s", name)
    traitNames(decl) = name  
  }

  def nameComplexTypes(decl: TaggedType[XComplexType]) {
    val name = makeProtectedComplexTypeName(decl)
    logger.debug("nameComplexTypes: named %s", name)
    names(decl) = name
    nameComplexTypeCompositors(decl)
  }

  def nameNamedGroup(tagged: Tagged[XNamedGroup]) {
    val name = makeProtectedNamedGroup(tagged)
    logger.debug("nameNamedGroup: named %s", name)
    names(tagged) = name

    val primary = tagged.primaryCompositor
    tagged.compositors foreach { c =>
      if (Some(c) == primary) names(c) = name
      else nameCompositor(c, false)
      splitLongSequence(c) map { _ foreach { nameCompositor(_, false) }}
    }
  }

  def nameComplexTypeCompositors(decl: TaggedType[XComplexType]) {
    val primarySequence = decl.primarySequence
    implicit val s = schema.unbound
    val cs: Seq[TaggedParticle[KeyedGroup]] = decl.toSeq collect {
      case Compositor(compositor) => compositor
    }
    logger.debug("nameComplexTypeCompositors: %s", cs map { _.tag.toString })
    cs foreach { c => nameCompositor(c, Some(c) == primarySequence) }
  }

  def nameCompositor(tagged: TaggedParticle[KeyedGroup], isPrimarySequence: Boolean) {
    logger.debug("nameCompositor: %s", tagged.tag)
    tagged.value.key match {
      case ChoiceTag   =>
        val name = makeProtectedTypeName(tagged.tag.name + "Option", "", tagged.tag, false)
        names(tagged) = name
        logger.debug("nameCompositor: named %s %s", tagged.tag, name)
      case SequenceTag =>
        if (isPrimarySequence) {
          Occurrence(tagged) match {
            case Occurrence(1, 1, _) =>
            case _ => 
              val name = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
              names(tagged) = name
              logger.debug("nameCompositor: named %s %s", tagged.tag, name)
          }
        }
        else {
          val name = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
          names(tagged) = name
          logger.debug("nameCompositor: named %s %s", tagged.tag, name)
        }
        
        self.splitLongSequence(tagged) map { _ map { seq =>
          nameCompositor(seq, false)
        }}
      case AllTag      =>
        val name = makeProtectedTypeName(tagged.tag.name + "All", "", tagged.tag, false)
        names(tagged) = name
        logger.debug("nameCompositor: named %s %s", tagged.tag, name)
      case _ => sys.error("unknown KeyedGroup" + tagged.toString)
    }
  }

  def nameEnumValues(decl: TaggedType[XSimpleType], scope: scala.xml.NamespaceBinding) {
    filterEnumeration(decl) map { enum =>
      val name = makeProtectedEnumTypeName(enum, decl, scope)
      names(enum) = name
      val value = decl.enumValue(enum)
      logger.debug("nameEnumValues: named %s: %s %s", value, value.getClass, name)
    }
  }

  def nameAttributeGroup(tagged: Tagged[XAttributeGroup]) {
    names(tagged) = makeProtectedAttributeGroupName(tagged)
  }

  def makeProtectedElementTypeName(elem: Tagged[XElement]): String =
    makeProtectedTypeName(elem.name, "", elem.tag, true)

  def makeProtectedComplexTypeName(decl: TaggedType[XComplexType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedSimpleTypeName(decl: TaggedType[XSimpleType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedEnumTypeName(enum: Tagged[XNoFixedFacet], decl: TaggedType[XSimpleType], scope: scala.xml.NamespaceBinding): String =
    makeProtectedTypeName(decl.enumValue(enum) match {
        case QualifiedName(ns, localPart) => Option(scope.getPrefix(ns map {_.toString} orNull)).getOrElse("") + localPart
        case v => v.toString
      },
      "Value", enum.tag, true)

  def makeProtectedNamedGroup(tagged: Tagged[XNamedGroup]): String =
    makeProtectedTypeName(tagged.name.get + "Group", "", tagged.tag, true)

  def makeProtectedAttributeGroupName(tagged: Tagged[XAttributeGroup]): String =
    makeProtectedTypeName(tagged.name.get, "", tagged.tag, true)
  
  def makeProtectedTypeName(initialName: Option[String], postfix: String, tag: HostTag, appendHost: Boolean): String =
    makeProtectedTypeName(initialName getOrElse {sys.error("name is required.")}, postfix, tag, appendHost)

  def makeProtectedTypeName(initialName: String, postfix: String, tag: HostTag, appendHost: Boolean): String = {
    def contains(s: String) = names.valuesIterator.contains(s)

    var name = makeTypeName(initialName)
    if (!contains(name)) name
    else {
      name = makeTypeName(tag.name + initialName.capitalize)
      if (appendHost && !contains(name) && initialName != tag.name) name
      else {
        name = makeTypeName(initialName) + postfix
        for (i <- 2 to 100) {
          if (contains(name)) name = makeTypeName(initialName) + postfix + i
        } // for i
        name
      }
    }
  }

  def makeParamName(name0: String) = {
    val name = name0.trim
    val base = config.paramPrefix map { p =>
      if (p.endsWith("_"))  p + name
      else p + name.capitalize
    } getOrElse { name }

    if (isKeyword(base)) identifier(base + "Value")
    else if (startsWithNumber(base)) identifier("number" + base)
    else identifier(base)
  }

  private def makeTypeName(name: String) = name match {
    case s if (s.startsWith("java.") || s.startsWith("javax.")) => s
    case _ =>
      val base = config.classPrefix map { p =>
        if (p.endsWith("_"))  p.capitalize + name
        else p.capitalize + name.capitalize
      } getOrElse { identifier(name).capitalize }
      if (startsWithNumber(base)) "Number" + base
      else if (isCommonlyUsedWord(base)) base + "Type"
      else base
  }

  private def startsWithNumber(name: String) =
    """\d""".r.findPrefixMatchOf(name) match {
      case Some(_) => true
      case _ => false
    }

  def identifier(value: String) = {
    val nonspace = 
      if (value.trim != "") """\s""".r.replaceAllIn(value, "")
      else value    
    if ("""\W""".r.findFirstIn(nonspace).isDefined) {
      (nonspace.toSeq map { c =>
        if ("""\W""".r.findFirstIn(c.toString).isDefined) "u" + c.toInt.toString
        else c.toString
      }).mkString
    }
    else nonspace
  }
  
  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }

  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""

  def indent(indent: Int) = "  " * indent

  def quoteUri(value: Option[URI]): String = quote(value map {_.toString})

  def optionTree(value: Option[String]): Tree =
    value map { x => SOME(LIT(x)) } getOrElse {NONE}

  def optionUriTree(value: Option[URI]): Tree = optionTree(value map {_.toString})
}
