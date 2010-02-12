/**
 * @author  e.e d3si9n
 */

package schema2src.xsd

import schema2src._
// import scala.xml.xsd.{ ElemDecl, TypeDecl, XsTypeSymbol }
import scala.collection.Map
import scala.xml._

class GenSource(conf: Driver.XsdConfig, schema: (Map[String, ElemDecl], Map[String, TypeDecl])) extends ScalaNames {
  import conf.{outfile => fOut, objName => objectName}
  
  val elems = schema._1
  val types = schema._2
  val newline = System.getProperty("line.separator")
  val defaultSuperName = "DataModel"

  def run {
    import scala.collection.mutable
    Main.log("xsd: GenSource.run")
    
    if (conf.packageName != null)
      myprintAll(makePackageName.child)
    
    myprintAll(makeParentClass.child)
    
    for (typePair <- types;
        if typePair._2.isInstanceOf[ComplexTypeDecl];
        if !typePair._1.contains("@"))
      myprintAll(makeType(typePair._2.asInstanceOf[ComplexTypeDecl]).child)
      
    for (elem <- elems.valuesIterator;
        val typeSymbol = elem.typeSymbol;
        if typeSymbol.name.contains("@");
        if typeSymbol.isInstanceOf[ReferenceTypeSymbol];
        val ref = typeSymbol.asInstanceOf[ReferenceTypeSymbol];
        if ref.decl.isInstanceOf[ComplexTypeDecl])
      myprintAll(makeElement(elem).child)
          
    myprintAll(makeHelperObject.child)
  }
  
  def makeElement(elem: ElemDecl): scala.xml.Node = elem.typeSymbol match {
    case ReferenceTypeSymbol(decl: ComplexTypeDecl)
      => makeCaseClassWithType(makeTypeName(elem.name), decl)
    case _ => throw new Exception("GenSource: Unsupported element " + elem.toString)
  }
  
  def makeType(decl: ComplexTypeDecl): scala.xml.Node =
    makeCaseClassWithType(makeTypeName(decl.name), decl)
    
  def makeCaseClassWithType(name: String, decl: ComplexTypeDecl): scala.xml.Node = {
    Main.log("GenSource: emitting " + name)
    
    val childElements = flattenContent(decl.content)
    val list = List.concat[Decl](childElements, flattenAttributes(decl))
    val paramList = list.map(buildParam(_))
    val argList = list.map(buildArg(_))
    val superInit = buildSuperInit(decl.content.content)
    
    return <source>
case class {name}({
  paramList.mkString("," + newline + indent(1))}) extends {superInit} {{
}}

object {name} {{
  def fromXML(node: scala.xml.Node) =
    {name}({argList.mkString("," + newline + indent(3))}) 
}}
</source>    
  }
  
  def buildSuperInit(content: ContentTypeDecl): String = content match {
    //case ExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
    //  makeTypeName(base.name)  
    case _ => defaultSuperName
  }
    
  def buildParam(decl: Decl): String = decl match {
    case elem: ElemDecl => buildParam(elem)
    case attr: AttributeDecl => buildParam(attr)
    case _ => throw new Exception("GenSource: unsupported delcaration " + decl.toString)
  }
  
  def buildParam(elem: ElemDecl): String = {
    if (elem.maxOccurs > 1) {
      elem.name + ": List[" + buildTypeName(elem.typeSymbol, elem.name) + "]"
    } else if (elem.minOccurs == 0) {
      elem.name + ": Option[" + buildTypeName(elem.typeSymbol, elem.name) + "]"
    } else {
      elem.name + ": " + buildTypeName(elem.typeSymbol, elem.name)
    }    
  }
  
  def buildParam(attr: AttributeDecl): String = 
    if (toMinOccurs(attr) == 0)
      attr.name + ": Option[" + buildTypeName(attr.typeSymbol, attr.name) + "]"
    else
      attr.name + ": " + buildTypeName(attr.typeSymbol, attr.name)
    
  
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
    val typeName = buildTypeName(elem.typeSymbol, elem.name)
    
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
  
  def buildTypeName(content: ContentTypeDecl): String = content match {
    case SimpTypRestrictionDecl(base: BuiltInSimpleTypeSymbol) => base.name
    
    case _ => throw new Exception("GenSource: Unsupported content " + content.toString)
  }
  
  def buildTypeName(typeSymbol: XsTypeSymbol, elemName: String): String = typeSymbol match {
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl.content)    
    case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
      if (!decl.name.contains("@"))
        makeTypeName(decl.name)
      else
        makeTypeName(elemName)
    case _ => throw new Exception("GenSource: Invalid type " + typeSymbol.toString + " in " + elemName)    
  }
    
  def flattenContent(content: HasContent): List[ElemDecl] = content match {
    case simple: SimpleContentDecl => Nil
    case complex: ComplexContentDecl => flattenComplexContent(complex)
  }
  
  def flattenComplexContent(content: ComplexContentDecl): List[ElemDecl] = content.content match {
    case CompContRestrictionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) => Nil
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) => Nil
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      flattenContent(base.content)        
    case res@CompContRestrictionDecl(xsAny, _, _) =>
      flattenElements(res.compositor)
      
    case CompContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _, _) => Nil
    case CompContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, _) => Nil
    case ext@CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, _) =>
      flattenContent(base.content) ::: flattenElements(ext.compositor)
    case ext@CompContExtensionDecl(xsAny, _, _) =>
      flattenElements(ext.compositor)
        
    case _ => Nil
  }
  
  def flattenElements(compositor: Option[HasParticle]): List[ElemDecl] = compositor match {
    case None => Nil
    case Some(c) => flattenElements(c)
  }
  
  def flattenElements(compositor: HasParticle): List[ElemDecl] = {
    val list = for (particle <- compositor.particles)
      yield particle match {
        case compositor2: HasParticle   => flattenElements(compositor2)
        case elem: ElemDecl             =>
          compositor match {
            case sequence: SequenceDecl => List(elem)
            case _                      => List(toOptional(elem))
          }
        case _ => throw new Exception("GenSource: Invalid content model" + particle.toString)
      }
    list.flatten
  }
    
  def flattenAttributes(decl: ComplexTypeDecl): List[AttributeDecl] = decl.content match {
    case simple: SimpleContentDecl => flattenAttributes(simple.content)
    case complex: ComplexContentDecl => flattenAttributes(complex.content)
  }
  
  def flattenAttributes(content: ContentTypeDecl): List[AttributeDecl] = content match {
    case CompContRestrictionDecl(symbol: BuiltInSimpleTypeSymbol, _, attr) => attr
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, attr) => attr
    case CompContRestrictionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) => 
      attr ::: flattenAttributes(base)
    case CompContRestrictionDecl(xsAny, _, attr) => attr
    
    case CompContExtensionDecl(symbol: BuiltInSimpleTypeSymbol, _, attr) => attr
    case CompContExtensionDecl(ReferenceTypeSymbol(base: SimpleTypeDecl), _, attr) => attr
    case CompContExtensionDecl(ReferenceTypeSymbol(base: ComplexTypeDecl), _, attr) =>
      attr ::: flattenAttributes(base)
    case CompContExtensionDecl(xsAny, _, attr) => attr
    
    case _ => Nil
  }
    
  def toOptional(that: ElemDecl) =
    ElemDecl(that.name, that.typeSymbol, that.defaultValue, that.fixedValue,
      0, that.maxOccurs)
      
  def makeTypeName(name: String) =
    if (name.contains("."))
      name
    else
      name.substring(0, 1).toUpperCase() + name.substring(1)

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
