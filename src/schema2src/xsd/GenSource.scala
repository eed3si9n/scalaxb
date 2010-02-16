/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

import schema2src._
// import scala.xml.xsd.{ ElemDecl, TypeDecl, XsTypeSymbol }
import scala.collection.Map
import scala.collection.mutable
import scala.xml._

class GenSource(conf: Driver.XsdConfig, schema: SchemaDecl) extends ScalaNames {
  import conf.{outfile => fOut, objName => objectName}
  
  val elems = schema.elems
  val types = schema.types
  val choices = schema.choices
  val newline = System.getProperty("line.separator")
  val defaultSuperName = "DataModel"
  val baseToSubs = mutable.Map.empty[ComplexTypeDecl, List[ComplexTypeDecl]]
  val choiceNames = mutable.Map.empty[ChoiceDecl, String]
  val typeNames = mutable.Map.empty[ComplexTypeDecl, String]
  val complexTypes = mutable.HashSet.empty[ComplexTypeDecl]
  val choiceWrapper = mutable.Map.empty[ComplexTypeDecl, ChoiceDecl]
  var argNumber = 0
  
  def run {
    import scala.collection.mutable
    Main.log("xsd: GenSource.run")
    
    if (conf.packageName != null)
      myprintAll(makePackageName.child)
    
    myprintAll(makeParentClass.child)
    
    for (elem <- elems.valuesIterator.toList;
        val typeSymbol = elem.typeSymbol;
        if typeSymbol.name.contains("@");
        if typeSymbol.isInstanceOf[ReferenceTypeSymbol];
        val ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol];
        if ref.decl.isInstanceOf[ComplexTypeDecl];
        val decl = ref.decl.asInstanceOf[ComplexTypeDecl]) {
      
      typeNames(decl) = elem.name.capitalize
      complexTypes += decl
    }
    
    for (typePair <- types;
        if typePair._2.isInstanceOf[ComplexTypeDecl];
        if !typePair._1.contains("@");
        val decl = typePair._2.asInstanceOf[ComplexTypeDecl]) {
      
      typeNames(decl) = makeProtectedTypeName(decl)
      complexTypes += decl
    }
    
    for (typ <- complexTypes)  typ.content.content match {
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
    
    for (typ <- complexTypes)
      if (baseToSubs.keysIterator.contains(typ))
        typeNames(typ) = makeTraitName(typ)
    
    var i = 0
    for (choice <- choices) {
      i += 1
      choiceNames(choice) = "choice" + i
    }
        
    for (base <- baseToSubs.keysIterator)
      myprintAll(makeSuperType(base).child)
    
    for (base <- baseToSubs.keysIterator)
      myprintAll(makeTrait(base).child)
    
    for (typ <- complexTypes)
      if (!baseToSubs.keysIterator.contains(typ))
        myprintAll(makeType(typ).child)
        
    for (choice <- choices)
      myprintAll(makeChoiceTrait(choice).child)
    
    myprintAll(makeHelperObject.child)
  }
  
  def makeTraitName(decl: ComplexTypeDecl) =
    if (decl.name.last == 'e')
      decl.name.dropRight(1) + "able"
    else
      decl.name + "able"
  
  def makeSuperType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(makeProtectedTypeName(decl), decl)
      
  def makeType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(typeNames(decl), decl)
  
  def associateSubType(subType: ComplexTypeDecl, base: ComplexTypeDecl) {
    if (baseToSubs.contains(base)) {
      baseToSubs(base) = subType :: baseToSubs(base)
    } else {
      baseToSubs(base) = subType :: Nil
    } // if-else
  }
    
  def makeProtectedTypeName(decl: ComplexTypeDecl): String =
    if (typeNames.valuesIterator.contains(decl.name))
      makeTypeName(decl.name) + "Type"
    else
      makeTypeName(decl.name)
  
  def makeTypeName(name: String) =
    if (name.contains("."))
      name
    else
      name.capitalize
      
  def buildTypeName(typeSymbol: XsTypeSymbol): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl.content)    
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => typeNames(decl)
    case _ => throw new Exception("GenSource: Invalid type " + typeSymbol.toString)    
  }
  
  def buildTypeName(content: ContentTypeDecl): String = content match {
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => base.name
    
    case _ => throw new Exception("GenSource: Unsupported content " + content.toString)
  }
  
  def makeChoiceTrait(choice: ChoiceDecl): scala.xml.Node = {
    val name = makeTypeName(choiceNames(choice))
    
    def makeCaseEntry(decl: Decl) = decl match {
      case elem: ElemDecl =>
        val name = makeTypeName(elem.name)
        val typeName = buildTypeName(elem.typeSymbol)
        "case <" + elem.name + ">_</" + elem.name + "> => " + typeName + ".fromXML(node)"
      case _ => throw new Exception("GenSource: Unsupported compositor " + decl.toString)
    }
    
    return <source>
trait {name}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} = node match {{
    {
      val cases = for (particle <- choice.particles)
        yield makeCaseEntry(particle)
      cases.mkString(newline + indent(2))        
    }    
  }}
}}
</source>    
  }
      
  def makeTrait(decl: ComplexTypeDecl): scala.xml.Node = {
    val name = typeNames(decl)
    Main.log("GenSource: emitting " + name)

    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    val superName = buildSuperName(decl)
    val defaultType = makeProtectedTypeName(decl)
    val options = buildOptions(decl)
    
    val extendString = if (options.isEmpty)
      ""
    else
      " extends " + options.mkString(" with ")
    
    def makeCaseEntry(decl: ComplexTypeDecl) = {
      val name = typeNames(decl)
      "case " + quote(name) + " => " + name + ".fromXML(node)"
    }
    
    return <source>
trait {name}{extendString} {{
  {
  val vals = for (param <- paramList)
    yield  "val " + param + ";"
  vals.mkString(newline + indent(1))}
}}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} = {{
    val typeName = (node \ "@{{http://www.w3.org/2001/XMLSchema-instance}}type").text    
    val withoutNS = typeName.drop(typeName.indexOf(":") + 1)
    
    withoutNS match {{
      {
        val cases = for (sub <- baseToSubs(decl))
          yield makeCaseEntry(sub)
        cases.mkString(newline + indent(3))        
      }
      case _ => {defaultType}.fromXML(node)
    }}
  }}
}}
</source>    
  }
        
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    Main.log("GenSource: emitting " + name)
    
    val superNames: List[String] = if (baseToSubs.contains(decl))
      List(defaultSuperName, typeNames(decl))
    else
      buildSuperNames(decl)
      
    val childElements = flattenElements(decl, name)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    
    def superNamesString =
      superNames.mkString(" with ")
    
    return <source>
case class {name}({
  paramList.mkString("," + newline + indent(1))}) extends {superNamesString} {{
}}

object {name} {{
  def fromXML(node: scala.xml.Node): {name} =
    {name}({argList.mkString("," + newline + indent(3))}) 
}}
</source>    
  }
  
  def buildSuperNames(decl: ComplexTypeDecl) = {
    val superName = buildSuperName(decl)
    if (superName == defaultSuperName)
      List(superName) ::: buildOptions(decl)
    else
      List(defaultSuperName, superName) ::: buildOptions(decl)
  }
  
  def buildOptions(decl: ComplexTypeDecl) = {
    val set = mutable.Set.empty[String]
    for (choice <- choices;
      particle <- choice.particles;
      if particle.isInstanceOf[ElemDecl];
      val elem = particle.asInstanceOf[ElemDecl];
      if elem.typeSymbol.isInstanceOf[ReferenceTypeSymbol];
      val ref = elem.typeSymbol.asInstanceOf[ReferenceTypeSymbol];
      if ref.decl == decl)
        set += makeTypeName(choiceNames(choice))
    set.toList
  }
  
  def buildSuperName(decl: ComplexTypeDecl): String = 
    decl.content.content.base match {
      case ReferenceTypeSymbol(base: ComplexTypeDecl) => typeNames(base)
      case _ => makeTypeName(decl.content.content.base.name) 
    }
  
  def buildParam(decl: Decl): String = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case _ => throw new Exception("GenSource: unsupported delcaration " + decl.toString)
  }
  
  def buildParam(elem: ElemDecl): String = {
    if (elem.maxOccurs > 1) {
      elem.name + ": List[" + buildTypeName(elem.typeSymbol) + "]"
    } else if (elem.minOccurs == 0) {
      elem.name + ": Option[" + buildTypeName(elem.typeSymbol) + "]"
    } else {
      elem.name + ": " + buildTypeName(elem.typeSymbol)
    }    
  }
  
  def buildParam(attr: AttributeDecl): String = 
    if (toMinOccurs(attr) == 0)
      attr.name + ": Option[" + buildTypeName(attr.typeSymbol) + "]"
    else
      attr.name + ": " + buildTypeName(attr.typeSymbol)
  
  def buildArg(decl: Decl): String = decl match {
    case elem: ElemDecl       => buildArg(elem)
    case attr: AttributeDecl  => buildArg(attr)
    case _ => throw new Exception("GenSource: unsupported delcaration " + decl.toString)
  }
  
  def buildArg(elem: ElemDecl): String = {
    val typeSymbol = elem.typeSymbol
    
    typeSymbol match {
      case symbol: BuiltInSimpleTypeSymbol => buildValueCode(symbol,
        elem.name, elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs)
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>  buildValueCode(decl,
        elem.name, elem.defaultValue, elem.fixedValue, elem.minOccurs, elem.maxOccurs)
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>  buildArg(elem, decl)
      case _ => throw new Exception("GenSource: Invalid type " + typeSymbol.toString)
    }
  }

  def buildArg(elem: ElemDecl, decl: ComplexTypeDecl): String = {
    val typeName = buildTypeName(elem.typeSymbol)
    
    if (elem.maxOccurs > 1) {
      "(node \\ \"" + elem.name + "\").toList.map(" + typeName + ".fromXML(_))" 
    } else if (elem.minOccurs == 0) {
      "(node \\ \"" + elem.name + "\").headOption match {" + newline +
      indent(4) + "case None    => None" + newline +
      indent(4) + "case Some(x) => Some(" +  typeName + ".fromXML(x))" + newline +
      indent(3) + "}"
    } else {
      typeName + ".fromXML((node \\ \"" + elem.name + "\").head)" 
    }    
  }
  
  def toMinOccurs(attr: AttributeDecl) = 
    if (attr.use == RequiredUse ||
      attr.fixedValue.isDefined ||
      attr.defaultValue.isDefined)
      1
    else
      0
  
  def buildArg(attr: AttributeDecl): String = attr.typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol =>
      buildValueCode(symbol, "@" + attr.name, attr.defaultValue, attr.fixedValue,
        toMinOccurs(attr), 1)
        
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
      buildValueCode(decl, "@" + attr.name, attr.defaultValue, attr.fixedValue,
        toMinOccurs(attr), 1)    
    
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      throw new Exception("GenSource: Attribute with complex type " + decl.toString) 
  }

  def buildValueCode(decl: SimpleTypeDecl, nodeName: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int): String = decl.content match {  
    
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) =>
      buildValueCode(base, nodeName, defaultValue, fixedValue, minOccurs, maxOccurs)
    
    case _ => throw new Exception("GenSource: Unsupported content " + decl.content.toString)    
  }
    
  def buildValueCode(typeSymbol: BuiltInSimpleTypeSymbol, nodeName: String,
      defaultValue: Option[String], fixedValue: Option[String],
      minOccurs: Int, maxOccurs: Int): String = {
        
    val stringValueCode = "(node \\ \"" + nodeName + "\").text"
    val (pre, post) = typeSymbol.name match {
      case "String"     => ("", "")
      case "javax.xml.datatype.Duration" => ("Helper.toDuration(", ")")
      case "java.util.Calendar" => ("Helper.toCalendar(", ")")
      case "Boolean"    => ("", ".toBoolean")
      case "Int"        => ("", ".toInt")
      case "Long"       => ("", ".toLong")
      case "Short"      => ("", ".toShort")
      case "Float"      => ("", ".toFloat")
      case "Double"     => ("", ".toDouble")
      case "Byte"       => ("", ".toByte")
      case "BigInt"     => ("BigInt(", ")")
      case "BigDecimal" => ("BigDecimal(", ")")
      case "java.net.URI" => ("java.net.URI.create(", ")")
      case "javax.xml.namespace.QName"
        => ("javax.xml.namespace.QName.valueOf(", ")")
      case "Array[String]" => ("", ".split(' ')")
      // case "Base64Binary" =>
      // case "HexBinary"  => 
      case _        => throw new Exception("GenSource: Unsupported type " + typeSymbol.toString) 
    }
    
    def buildMatchStatement(noneValue: String, someValue: String) =
      "(node \\ \"" + nodeName + "\").headOption match {" + newline +
      indent(4) + "case None    => " + noneValue + newline +
      indent(4) + "case Some(x) => " + someValue + newline +
      indent(3) + "}"
    
    if (maxOccurs > 1) {
      "(node \\ \"" + nodeName + "\").toList.map(" + pre + "_.text" + post + ")"
    } else if (minOccurs == 0) {
      buildMatchStatement("None", "Some(" + pre + "x.text" + post + ")")
    } else if (defaultValue.isDefined) {
      buildMatchStatement(pre + quote(defaultValue.get) + post, pre + "x.text" + post)
    } else if (fixedValue.isDefined) {
      pre + quote(fixedValue.get) + post
    } else {
      pre + "(node \\ \"" + nodeName + "\").text" + post
    }    
  }
  
  def quote(value: String) = "\"" + value + "\""
      
  def flattenElements(decl: ComplexTypeDecl, name: String): List[ElemDecl] = {
    argNumber = 0
    
    decl.content.content match {
      case CompContRestrictionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) => Nil
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) => Nil
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name))        
      case res@CompContRestrictionDecl(xsAny, _, _) =>
        flattenElements(res.compositor, name)
      
      case CompContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) => Nil
      case CompContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) => Nil
      case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
        flattenElements(base, makeTypeName(base.name)) :::
          flattenElements(ext.compositor, name)
      case ext@CompContExtensionDecl(xsAny, _, _) =>
        flattenElements(ext.compositor, name)
        
      case _ => Nil
    }
  }
  
  def flattenElements(compositor: Option[HasParticle], name: String): List[ElemDecl] =
      compositor match {
    case None => Nil
    case Some(c) => flattenElements(c, name)
  }
  
  def flattenElements(compositor: HasParticle, name: String): List[ElemDecl] =
      compositor match {
    case SequenceDecl(particles: List[Decl]) =>
      val list = for (particle <- particles)
        yield particle match {
          case compositor2: HasParticle => flattenElements(compositor2, "")
          case elem: ElemDecl           => List(elem)
        }
      list.flatten
    
    case AllDecl(particles: List[Decl]) =>
      val list = for (particle <- particles)
        yield particle match {
          case compositor2: HasParticle => flattenElements(compositor2, "")
          case elem: ElemDecl           => List(toOptional(elem))
        }
      list.flatten
    
    case choice@ChoiceDecl(particles: List[Decl]) =>
      List(buildChoiceRef(choice, name))
      
      /*
      val list = for (particle <- particles)
        yield particle match {
          case compositor2: HasParticle => flattenElements(compositor2, "")
          case elem: ElemDecl           => List(toOptional(elem))
        }
      list.flatten
      */
  }
  
  def buildChoiceRef(choice: ChoiceDecl, parentName: String) = {    
    val symbol = new ReferenceTypeSymbol(makeTypeName(choiceNames(choice)))
    val decl = ComplexTypeDecl(symbol.name, null, Nil)
    
    choiceWrapper(decl) = choice
    
    symbol.decl = decl
    typeNames(decl) = makeTypeName(choiceNames(choice)) 
    argNumber += 1
    val name = "arg" + argNumber  
    ElemDecl(name, symbol, None, None, 1, 1)
  }
  
  def toOptional(that: ElemDecl) = {
    ElemDecl(that.name, that.typeSymbol, that.defaultValue, that.fixedValue,
      0, that.maxOccurs)
  }
        
  def flattenAttributes(decl: ComplexTypeDecl): List[AttributeDecl] =
    decl.content.content.attributes ::: (decl.content.content match {
      case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) => 
        flattenAttributes(base)
      case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) =>
        flattenAttributes(base)
      
      case _ => List()
    })
  
  def myprintAll(nodes: Seq[Node]) {
    for (node <- nodes)
      myprint(node)
  }
  
  def myprint(n: Node) = n match {
    case Text(s)          => fOut.print(s)
    case EntityRef("lt")  => fOut.print('<')
    case EntityRef("gt")  => fOut.print('>')
    case EntityRef("amp") => fOut.print('&')
    case atom: Atom[_]    => fOut.print(atom.text)
    case _                => Main.log("error in xsd:run: encountered "
      + n.getClass() + " " + n.toString)
  }
  
  def makePackageName = {
    <source>package {conf.packageName}
</source>
  }
  
  def makeParentClass = {
    <source>
abstract class {defaultSuperName}
</source>    
  }
  
  def makeHelperObject = {
    <source>
class Calendar extends java.util.GregorianCalendar {{
  override def toString: String = Helper.toString(this)
}}

object Calendar {{
  def apply(value: String): Calendar = Helper.toCalendar(value)
  def unapply(value: Calendar): Option[String] = Some(Helper.toString(value))
}}

object Helper {{
  lazy val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()

  def toCalendar(value: String) = {{
    val gregorian = typeFactory.newXMLGregorianCalendar(value).toGregorianCalendar
    val cal = new Calendar()

    for (i &lt;- 0 to java.util.Calendar.FIELD_COUNT - 1)
      if (gregorian.isSet(i))
        cal.set(i, gregorian.get(i))
    cal
  }}

  def toString(value: Calendar) = {{
    val xmlGregorian = typeFactory.newXMLGregorianCalendar(value)
    xmlGregorian.toString
  }}

  def toDuration(value: String) =
    typeFactory.newDuration(value)
}}
</source>    
  }

  def indent(indent: Int) = "  " * indent
}
