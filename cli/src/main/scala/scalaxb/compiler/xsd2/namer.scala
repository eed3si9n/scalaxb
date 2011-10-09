package scalaxb.compiler.xsd2

import java.net.URI
import xmlschema._
import scalaxb.compiler.{ScalaNames}
import Defs._

trait PackageNamer {
  def packageNameByURI(namespace: Option[URI], context: SchemaContext): Option[String] =
    packageName(namespace map {_.toString}, context)

  def packageName(namespace: Option[String], context: SchemaContext): Option[String] =
    if (context.packageNames.contains(namespace)) context.packageNames(namespace)
    else if (context.packageNames.contains(None)) context.packageNames(None)
    else None
}

trait Namer extends ScalaNames { self: Lookup with Splitter  =>
  def nameElementTypes(elem: Tagged[XElement]) {
    implicit val tag = elem.tag

    elem.xelementoption map {_.value match {
      case x: XLocalComplexType =>
        names(Tagged(x, tag)) = makeProtectedElementTypeName(elem)
      case x: XLocalSimpleType if (containsEnumeration(x)) =>
        names(Tagged(x, tag)) = makeProtectedElementTypeName(elem)
        nameEnumValues(Tagged(x, elem.tag))
      case _ =>
    }}
  }

  def nameSimpleTypes(decl: Tagged[XSimpleType]) {
    if (containsEnumeration(decl)) {
      names(decl) = makeProtectedSimpleTypeName(decl)
      nameEnumValues(decl)
    }
  }

  def nameComplexTypes(decl: Tagged[XComplexType]) {
    names(decl) = makeProtectedComplexTypeName(decl)
    val primarySequence = decl.primarySequence
    decl collect {
      case Compositor(compositor) => nameCompositor(compositor, Some(compositor) == primarySequence)
    }
  }

  def nameCompositor(tagged: Tagged[KeyedGroup], isPrimarySequence: Boolean) {
    tagged.value.key match {
      case ChoiceTag   => names(tagged) = makeProtectedTypeName(tagged.tag.name + "Option", "", tagged.tag, false)
      case SequenceTag =>
        if (isPrimarySequence) {
          Occurrence(tagged) match {
            case Occurrence(1, 1, _) =>
            case _ => names(tagged) = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
          }
        }
        else names(tagged) = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
        self.splitLongSequence(tagged) map { _ map { seq =>
          names(seq) = makeProtectedTypeName(seq.tag.name + "Sequence", "", seq.tag, false)
        }}
      case AllTag      => names(tagged) = makeProtectedTypeName(tagged.tag.name + "All", "", tagged.tag, false)
      case _ =>
    }
  }

  def nameEnumValues(decl: Tagged[XSimpleType]) {
    filterEnumeration(decl) map { enum =>
      names(enum) = makeProtectedEnumTypeName(enum)
    }
  }

  def makeProtectedElementTypeName(elem: Tagged[XElement]): String =
    makeProtectedTypeName(elem.name, "", elem.tag, true)

  def makeProtectedComplexTypeName(decl: Tagged[XComplexType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedSimpleTypeName(decl: Tagged[XSimpleType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedEnumTypeName(enum: Tagged[XNoFixedFacet]): String =
    makeProtectedTypeName(enum.value.value, "Value", enum.tag, true)

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
}
