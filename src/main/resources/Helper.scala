package org.scalaxb.rt

import scala.xml.{Node, NodeSeq, NamespaceBinding, Elem}

trait XMLWriter[A] {
  implicit val ev = this
  def toXML(__obj: A, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq
}

object XMLWriter {
  implicit object __NodeXMLWriter extends XMLWriter[Node] {
    def toXML(__obj: Node, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq = __obj
  }

  implicit object __NodeSeqXMLWriter extends XMLWriter[NodeSeq] {
    def toXML(__obj: NodeSeq, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq = __obj
  }
  
  implicit object __ElemXMLWriter extends XMLWriter[Elem] {
    def toXML(__obj: Elem, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding): NodeSeq = __obj
  }  

  implicit object __StringXMLWriter extends XMLWriter[String] {
    def toXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, __namespace, __elementLabel, __scope)
  }

  implicit object __IntXMLWriter extends XMLWriter[Int] {
    def toXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteXMLWriter extends XMLWriter[Byte] {
    def toXML(__obj: Byte, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __ShortXMLWriter extends XMLWriter[Short] {
    def toXML(__obj: Short, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __LongXMLWriter extends XMLWriter[Long] {
    def toXML(__obj: Long, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigDecimalXMLWriter extends XMLWriter[BigDecimal] {
    def toXML(__obj: BigDecimal, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigIntXMLWriter extends XMLWriter[BigInt] {
    def toXML(__obj: BigInt, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __FloatXMLWriter extends XMLWriter[Float] {
    def toXML(__obj: Float, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __DoubleXMLWriter extends XMLWriter[Double] {
    def toXML(__obj: Double, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __BooleanXMLWriter extends XMLWriter[Boolean] {
    def toXML(__obj: Boolean, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __DurationXMLWriter extends XMLWriter[javax.xml.datatype.Duration] {
    def toXML(__obj: javax.xml.datatype.Duration, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __GregorianCalendarXMLWriter extends XMLWriter[java.util.GregorianCalendar] {
    def toXML(__obj: java.util.GregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), __namespace, __elementLabel, __scope)
  }
  
  // implicit object __HexBinaryXMLWriter extends XMLWriter[HexBinary] {
  //   def toXML(__obj: HexBinary, __namespace: Option[String], __elementLabel: Option[String],
  //       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
  //     Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  // }
  
  implicit object __QNameXMLWriter extends XMLWriter[javax.xml.namespace.QName] {
    def toXML(__obj: javax.xml.namespace.QName, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object __StringArrayXMLWriter extends XMLWriter[Array[String]] {
    def toXML(__obj: Array[String], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.mkString(" "), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteArrayXMLWriter extends XMLWriter[Array[Byte]] {
    def toXML(__obj: Array[Byte], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), __namespace, __elementLabel, __scope)
  }
  
  implicit object __URIXMLWriter extends XMLWriter[java.net.URI] {
    def toXML(__obj: java.net.URI, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  }
  
  implicit object AnyXMLWriter extends XMLWriter[Any] {
    def toXML(__obj: Any, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, __namespace, __elementLabel, __scope)
  } 
}

trait DataRecord[+A] {
  val namespace: Option[String]
  val key: Option[String]
  val value: A
  
  override def toString: String = {
    "DataRecord(" +
    ((namespace, key, value) match {
      case (None, Some(k), _) => k + "," + value.toString
      case (Some(n), Some(k), _) => "{" + n + "}" + k + "," + value.toString
      case _ => value.toString
    }) + ")"
  }
}

object DataRecord {
  private case class DataWriter[+A](
    namespace: Option[String],
    key: Option[String],
    value: A,
    writer: XMLWriter[_]) extends DataRecord[A]
  
  def apply[A:XMLWriter](namespace: Option[String], key: Option[String], value: A): DataRecord[A] =
    DataWriter(namespace, key, value, implicitly[XMLWriter[A]])
  
  def apply[A:XMLWriter](value: A): DataRecord[A] =
    apply(None, None, value)
  
  def unapply[A](record: DataRecord[A]): Option[(Option[String], Option[String], A)] =
    Some(record.namespace, record.key, record.value)
  
  def toXML[A](__obj: DataRecord[A], __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = __obj match {
    case w: DataWriter[_] => w.writer.asInstanceOf[XMLWriter[A]].toXML(__obj.value, __namespace, __elementLabel, __scope)
    case _ => error("unknown DataRecord.")
  }
}

case class ElemName(namespace: Option[String], name: String) {
  var node: scala.xml.Node = _
  def text = node.text
  def nil = Helper.isNil(node)
  def toDataRecord = DataRecord(namespace, Some(name), node)
}

object ElemName {
  implicit def toNode(elemName: ElemName): scala.xml.Node = elemName.node
}

trait AnyElemNameParser extends scala.util.parsing.combinator.Parsers {
  type Elem = ElemName
  
  def any: Parser[ElemName] = 
    accept("any", { case x: ElemName if x.name != "" => x })  
  
  def optTextRecord = opt(text ^^ (x => DataRecord(x.node.text)))
  
  def text: Parser[ElemName] =
    accept("text", { case x: ElemName if x.name == "" => x })
}

trait ElemNameParser[A] extends AnyElemNameParser with XMLWriter[A] {
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

// class HexBinary {
//   val Array[Byte]
// }

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
    
  def stringToXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {
    __elementLabel map { label =>
      scala.xml.Elem(getPrefix(__namespace, __scope).orNull, label,
        scala.xml.Null, __scope, scala.xml.Text(__obj.toString))
    } getOrElse { scala.xml.Text(__obj) }
  }
}
