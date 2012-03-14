package scalaxb.compiler.xsd2

import java.net.URI
import com.codahale.logula.Log
import xmlschema._
import scalaxb.compiler.{ScalaNames}
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

  def nameElementTypes(tagged: Tagged[XElement]) {
    tagged.localType map { _ match {
      case x: TaggedComplexType =>
        val name = makeProtectedElementTypeName(tagged)
        names(x) = name
        logger.debug("nameElementTypes: named %s", name)
        nameCompositors(x)
      case x: TaggedSimpleType =>
        val name = makeProtectedElementTypeName(tagged)
        logger.debug("nameElementTypes: named %s", name)
        names(x) = name
        nameEnumValues(x)
      case _ =>
    }}
  }

  def nameSimpleTypes(decl: Tagged[XSimpleType]) {
    if (containsEnumeration(decl)) {
      val name = makeProtectedSimpleTypeName(decl)
      logger.debug("nameSimpleTypes: named %s", name)
      names(decl) = name
      nameEnumValues(decl)
    }
  }

  def nameComplexTypes(decl: Tagged[XComplexType]) {
    val name = makeProtectedComplexTypeName(decl)
    logger.debug("nameComplexTypes: named %s", name)
    names(decl) = name
    nameCompositors(decl)
  }

  def nameCompositors(decl: Tagged[XComplexType]) {
    val primarySequence = decl.primarySequence
    implicit val s = schema.unbound
    decl collect {
      case Compositor(compositor) => nameCompositor(compositor, Some(compositor) == primarySequence)
    }
  }

  def nameCompositor(tagged: TaggedParticle[KeyedGroup], isPrimarySequence: Boolean) {
    // logger.debug("nameCompositor: %s", tagged.toString)
    tagged.value.key match {
      case ChoiceTag   =>
        val name = makeProtectedTypeName(tagged.tag.name + "Option", "", tagged.tag, false)
        names(tagged) = name
        logger.debug("nameCompositor: named %s", name)
      case SequenceTag =>
        if (isPrimarySequence) {
          Occurrence(tagged) match {
            case Occurrence(1, 1, _) =>
            case _ => 
              val name = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
              names(tagged) = name
              logger.debug("nameCompositor: named %s", name)
          }
        }
        else {
          val name = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
          names(tagged) = name
          logger.debug("nameCompositor: named %s", name)
        }
        
        self.splitLongSequence(tagged) map { _ map { seq => nameCompositor(seq, false)
        }}
      case AllTag      =>
        val name = makeProtectedTypeName(tagged.tag.name + "All", "", tagged.tag, false)
        names(tagged) = name
        logger.debug("nameCompositor: named %s", name)
      case _ => error("unknown KeyedGroup" + tagged.toString)
    }

    implicit val tag = tagged.tag
    tagged.particles collect {
      case Compositor(compositor) => nameCompositor(compositor, false)
    }
  }

  def nameEnumValues(decl: Tagged[XSimpleType]) {
    filterEnumeration(decl) map { enum =>
      names(enum) = makeProtectedEnumTypeName(enum)
    }
  }

  def nameAttributeGroup(tagged: Tagged[XAttributeGroup]) {
    names(tagged) = makeProtectedAttributeGroupName(tagged)
  }

  def makeProtectedElementTypeName(elem: Tagged[XElement]): String =
    makeProtectedTypeName(elem.name, "", elem.tag, true)

  def makeProtectedComplexTypeName(decl: Tagged[XComplexType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedSimpleTypeName(decl: Tagged[XSimpleType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedEnumTypeName(enum: Tagged[XNoFixedFacet]): String =
    makeProtectedTypeName(enum.value.value, "Value", enum.tag, true)

  def makeProtectedAttributeGroupName(tagged: Tagged[XAttributeGroup]): String =
    makeProtectedTypeName(tagged.name.get, "", tagged.tag, true);
  
  def makeProtectedTypeName(initialName: Option[String], postfix: String, tag: HostTag, appendHost: Boolean): String =
    makeProtectedTypeName(initialName getOrElse {error("name is required.")}, postfix, tag, appendHost)

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

  def makeParamName(name: String) = {
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

  private def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")

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

  def INFIX_CHAIN(op: String, seq: Iterable[Tree]): Tree =
    seq.toList match {
      case Nil => EmptyTree
      case List(x) => x
      case xs => xs reduceLeft { (x, y) => x INFIX(op) APPLY y }
    }
}
