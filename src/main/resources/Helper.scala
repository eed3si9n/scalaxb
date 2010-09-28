package org.scalaxb.rt

import scala.xml.{NodeSeq, NamespaceBinding}

trait XMLWriter {
  def toXML(__namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq
}

trait ImplicitXMLWriter[A] { outer =>
  def toXML(__obj: A, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): scala.xml.NodeSeq
  
  implicit def toXMLWriter(__x: A): XMLWriter = new XMLWriter {
     def toXML(__namespace: Option[String], __elementLabel: Option[String],
         __scope: NamespaceBinding): scala.xml.NodeSeq =
      outer.toXML(__x, __namespace, __elementLabel, __scope)
  }
}

case class DataRecord[+A](namespace: Option[String], key: Option[String], value: A, writer: Option[XMLWriter])

object DataRecord {
  def toXML[A](__obj: DataRecord[A], __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = __obj.writer match {
    case Some(w) => w.toXML(__namespace, __elementLabel, __scope)
    case _ => __obj.value match {
      case seq: NodeSeq => seq
      case x => Helper.anyToXML(x, __namespace, __elementLabel, __scope)
    }    
  }
}

case class ElemName(namespace: Option[String], name: String) {
  var node: scala.xml.Node = _
  def text = node.text
  def nil = Helper.isNil(node)
  def toDataRecord = DataRecord(namespace, Some(name), node, None)
}

object ElemName {
  implicit def toNode(elemName: ElemName): scala.xml.Node = elemName.node
}

trait AnyElemNameParser extends scala.util.parsing.combinator.Parsers {
  type Elem = ElemName
  
  def any: Parser[ElemName] = 
    accept("any", { case x: ElemName if x.name != "" => x })  
  
  def optTextRecord = opt(text ^^ (x => DataRecord(None, None, x.node.text, None)))
  
  def text: Parser[ElemName] =
    accept("text", { case x: ElemName if x.name == "" => x })
}

trait ElemNameParser[A] extends AnyElemNameParser with ImplicitXMLWriter[A] {
  def fromXML(seq: scala.xml.NodeSeq): A = seq match {
    case node: scala.xml.Node => fromXML(node)
    case _ => error("fromXML failed: seq must be scala.xml.Node")
  }
  
  def fromXML(node: scala.xml.Node): A =
    parse(parser(node), node.child) match {
      case x: Success[_] => x.get
      case x: Failure => error("fromXML failed: " + x.msg)
      case x: Error => error("fromXML errored: " + x.msg)
    }
  
  def parser(node: scala.xml.Node): Parser[A]
  def isMixed: Boolean
  
  def parse[A](p: Parser[A], in: Seq[scala.xml.Node]): ParseResult[A] = 
    p(new ElemNameSeqReader(elementNames(in)))
  
  def elementNames(in: Seq[scala.xml.Node]): Seq[ElemName] =
    if (isMixed) in map { toElemName }
    else in collect { case x: scala.xml.Elem => toElemName(x) }
    
  def toElemName(in: scala.xml.Node) = in match {
    case x: scala.xml.Elem =>    
      val elemName = ElemName(Option[String](x.scope.getURI(x.prefix)), x.label)
      elemName.node = x
      elemName
    case _ =>
      val elemName = ElemName(None, "")
      elemName.node = in
      elemName      
  }
}

class ElemNameSeqReader(seq: Seq[ElemName],
    override val offset: Int) extends scala.util.parsing.input.Reader[ElemName] {
  import scala.util.parsing.input._
  
  def this(seq: Seq[ElemName]) = this(seq, 0)
  
  override def first: ElemName  =
    if (seq.isDefinedAt(offset)) seq(offset)
    else null
    
  def rest: ElemNameSeqReader =
    if (seq.isDefinedAt(offset)) new ElemNameSeqReader(seq, offset + 1)
    else this
  
  def pos: Position = new ElemNameSeqPosition(seq, offset)
  
  def atEnd = !seq.isDefinedAt(offset)
  
  override def drop(n: Int): ElemNameSeqReader = 
    new ElemNameSeqReader(seq, offset + n)
}

class ElemNameSeqPosition(val source: Seq[ElemName], val offset: Int) extends 
    scala.util.parsing.input.Position {
  protected def lineContents =
    source.toString
    
  override def line = 1
  override def column = offset + 1
}

class Calendar extends java.util.GregorianCalendar {
  override def toString: String = Helper.toString(this)
}

object Calendar {
  def apply(value: String): Calendar = Helper.toCalendar(value)
  def unapply(value: Calendar): Option[String] = Some(Helper.toString(value))
}

object Helper {
  val XSI_URL = "http://www.w3.org/2001/XMLSchema-instance"
  val XSI_PREFIX = "xsi"
  lazy val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()

  def toCalendar(value: String) = {
    val gregorian = typeFactory.newXMLGregorianCalendar(value).toGregorianCalendar
    val cal = new Calendar()

    for (i <- 0 to java.util.Calendar.FIELD_COUNT - 1)
      if (gregorian.isSet(i))
        cal.set(i, gregorian.get(i))
    cal
  }
  
  def toString(value: java.util.GregorianCalendar) = {
    val xmlGregorian = typeFactory.newXMLGregorianCalendar(value)
    xmlGregorian.toString
  }
  
  def toString(value: Array[Byte]) =
    (new sun.misc.BASE64Encoder()).encodeBuffer(value)
  
  def toDuration(value: String) =
    typeFactory.newDuration(value)
  
  def toByteArray(value: String) =
    (new sun.misc.BASE64Decoder()).decodeBuffer(value)
    
  def toURI(value: String) =
    java.net.URI.create(value)
    
  def isNil(node: scala.xml.Node) =
    (node \ ("@{" + XSI_URL + "}nil")).headOption map { _.text == "true" } getOrElse {
      false
    }
  
  def nilElem(namespace: Option[String], elementLabel: String,
      scope: scala.xml.NamespaceBinding) =
    scala.xml.Elem(getPrefix(namespace, scope).orNull, elementLabel,
      scala.xml.Attribute(scope.getPrefix(XSI_URL), "nil", "true", scala.xml.Null),
      scope, Nil: _*)
      
  def instanceType(node: scala.xml.Node) = {
    val typeName = (node \ ("@{" + XSI_URL + "}type")).text
    val prefix = if (typeName.contains(':'))
      Some(typeName.dropRight(typeName.length - typeName.indexOf(':')))
      else None
    val namespace = Option[String](node.scope.getURI(prefix.orNull))
    val value = if (typeName.contains(':')) typeName.drop(typeName.indexOf(':') + 1)
      else typeName
    (namespace, value)
  }
  
  def getPrefix(namespace: Option[String], scope: scala.xml.NamespaceBinding) =
    if (Option[String](scope.getURI(null)) == namespace) None
    else Option[String](scope.getPrefix(namespace.orNull))
    
  def anyToXML(__obj: Any, __namespace: Option[String], __elementLabel: Option[String],
       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {
    __elementLabel map { label =>
      scala.xml.Elem(getPrefix(__namespace, __scope).orNull, label,
        scala.xml.Null, __scope, scala.xml.Text(__obj.toString))
    } getOrElse { scala.xml.Text(__obj.toString) }
  }
  
  def toXMLWriter(__obj: NodeSeq) = new XMLWriter {
    def toXML(__namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq = __obj
  }

  def toXMLWriter(__obj: Any) = new XMLWriter {
    def toXML(__namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.anyToXML(__obj, __namespace, __elementLabel, __scope)
  }
}
