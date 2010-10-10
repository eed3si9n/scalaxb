package org.scalaxb.rt

import scala.xml.{Node, NodeSeq, NamespaceBinding, Elem}
import javax.xml.datatype.{XMLGregorianCalendar}

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
      Helper.stringToXML(__obj, None, __namespace, __elementLabel, __scope)
  }
  
  object XSStringXMLWriter extends XMLWriter[String] {
    def toXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, Some("string"), __namespace, __elementLabel, __scope)
  }
  
  def stringXMLWriter(xstype: String) = new XMLWriter[String] {
    def toXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, Some(xstype), __namespace, __elementLabel, __scope)
  }

  implicit object __IntXMLWriter extends XMLWriter[Int] {
    def toXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSIntXMLWriter extends XMLWriter[Int] {
    def toXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("int"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteXMLWriter extends XMLWriter[Byte] {
    def toXML(__obj: Byte, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSByteXMLWriter extends XMLWriter[Byte] {
    def toXML(__obj: Byte, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("byte"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ShortXMLWriter extends XMLWriter[Short] {
    def toXML(__obj: Short, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSShortXMLWriter extends XMLWriter[Short] {
    def toXML(__obj: Short, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("short"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __LongXMLWriter extends XMLWriter[Long] {
    def toXML(__obj: Long, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def longXMLWriter(xstype: String) = new XMLWriter[Long] {
    def toXML(__obj: Long, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigDecimalXMLWriter extends XMLWriter[BigDecimal] {
    def toXML(__obj: BigDecimal, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDecimalXMLWriter extends XMLWriter[BigDecimal] {
    def toXML(__obj: BigDecimal, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("decimal"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigIntXMLWriter extends XMLWriter[BigInt] {
    def toXML(__obj: BigInt, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def bigIntXMLWriter(xstype: String) = new XMLWriter[BigInt] {
    def toXML(__obj: BigInt, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  def intXMLWriter(xstype: String) = new XMLWriter[Int] {
    def toXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __FloatXMLWriter extends XMLWriter[Float] {
    def toXML(__obj: Float, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSFloatXMLWriter extends XMLWriter[Float] {
    def toXML(__obj: Float, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("float"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __DoubleXMLWriter extends XMLWriter[Double] {
    def toXML(__obj: Double, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDoubleXMLWriter extends XMLWriter[Double] {
    def toXML(__obj: Double, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("double"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BooleanXMLWriter extends XMLWriter[Boolean] {
    def toXML(__obj: Boolean, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSBooleanXMLWriter extends XMLWriter[Boolean] {
    def toXML(__obj: Boolean, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("boolean"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __DurationXMLWriter extends XMLWriter[javax.xml.datatype.Duration] {
    def toXML(__obj: javax.xml.datatype.Duration, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDurationXMLWriter extends XMLWriter[javax.xml.datatype.Duration] {
    def toXML(__obj: javax.xml.datatype.Duration, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("duration"), __namespace, __elementLabel, __scope)
  }

  implicit object __CalendarXMLWriter extends XMLWriter[XMLGregorianCalendar] {
    def toXML(__obj: XMLGregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toXMLFormat, None, __namespace, __elementLabel, __scope)
  }
  
  def calendarXMLWriter(xstype: String) = new XMLWriter[XMLGregorianCalendar] {
    def toXML(__obj: XMLGregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toXMLFormat, Some(xstype), __namespace, __elementLabel, __scope)
  }
    
  implicit object __GregorianCalendarXMLWriter extends XMLWriter[java.util.GregorianCalendar] {
    def toXML(__obj: java.util.GregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toCalendar(__obj).toXMLFormat, None, __namespace, __elementLabel, __scope)
  }
  
  // implicit object __HexBinaryXMLWriter extends XMLWriter[HexBinary] {
  //   def toXML(__obj: HexBinary, __namespace: Option[String], __elementLabel: Option[String],
  //       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
  //     Helper.stringToXML(__obj.toString, "hexBinary", __namespace, __elementLabel, __scope)
  // }
  
  implicit object __QNameXMLWriter extends XMLWriter[javax.xml.namespace.QName] {
    def toXML(__obj: javax.xml.namespace.QName, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def qnameXMLWriter(xstype: String) = new XMLWriter[javax.xml.namespace.QName] {
    def toXML(__obj: javax.xml.namespace.QName, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __StringArrayXMLWriter extends XMLWriter[Array[String]] {
    def toXML(__obj: Array[String], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.mkString(" "), None, __namespace, __elementLabel, __scope)
  }
  
  def stringArrayXMLWriter(xstype: String) = new XMLWriter[Array[String]] {
    def toXML(__obj: Array[String], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.mkString(" "), Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteArrayXMLWriter extends XMLWriter[Array[Byte]] {
    def toXML(__obj: Array[Byte], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), None, __namespace, __elementLabel, __scope)
  }
  
  def byteArrayXMLWriter(xstype: String) = new XMLWriter[Array[Byte]] {
    def toXML(__obj: Array[Byte], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __URIXMLWriter extends XMLWriter[java.net.URI] {
    def toXML(__obj: java.net.URI, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSAnyURIXMLWriter extends XMLWriter[java.net.URI] {
    def toXML(__obj: java.net.URI, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("anyURI"), __namespace, __elementLabel, __scope)
  }
    
  implicit object AnyXMLWriter extends XMLWriter[Any] {
    def toXML(__obj: Any, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
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
  import Helper._
  import XMLWriter.__NodeSeqXMLWriter
    
  def apply[A:XMLWriter](namespace: Option[String], key: Option[String], value: A): DataRecord[A] =
    DataWriter(namespace, key, value, implicitly[XMLWriter[A]])
  
  def apply[A:XMLWriter](value: A): DataRecord[A] =
    apply(None, None, value)
  
  def apply(elemName: ElemName): DataRecord[Any] =
    if (elemName.name == "") apply(elemName.text)
    else instanceType(elemName.node) match {
      case (Some(XML_SCHEMA_URI), xstype)   =>
        xstype match {
          case "int"      => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.XSIntXMLWriter)
          case "byte"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toByte, XMLWriter.XSByteXMLWriter)
          case "short"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toShort, XMLWriter.XSShortXMLWriter)
          case "long"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toLong, XMLWriter.longXMLWriter(xstype))
          case "float"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toFloat, XMLWriter.XSFloatXMLWriter)
          case "double"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toDouble, XMLWriter.XSDoubleXMLWriter)
          case "integer"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "nonPositiveInteger" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "negativeInteger"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "nonNegativeInteger" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "positiveInteger"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "unsignedLong"    => DataWriter(elemName.namespace, Some(elemName.name), BigInt(elemName.text), XMLWriter.bigIntXMLWriter(xstype))
          case "unsignedInt"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toLong, XMLWriter.longXMLWriter(xstype))
          case "unsignedShort"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "unsignedByte"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLWriter.intXMLWriter(xstype))
          case "decimal"  => DataWriter(elemName.namespace, Some(elemName.name), BigDecimal(elemName.text), XMLWriter.XSDecimalXMLWriter)
          case "boolean"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toBoolean, XMLWriter.XSBooleanXMLWriter)
          case "string"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.XSStringXMLWriter)
          case "normalizedString" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "token"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "language" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "Name"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "NCName"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "NMTOKEN"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "NMTOKENS" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.split(' '), XMLWriter.stringArrayXMLWriter(xstype))
          case "ID"       => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "IDREF"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "IDREFS"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.split(' '), XMLWriter.stringArrayXMLWriter(xstype))
          case "ENTITY"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLWriter.stringXMLWriter(xstype))
          case "ENTITIES" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.split(' '), XMLWriter.stringArrayXMLWriter(xstype))
          case "base64Binary" => DataWriter(elemName.namespace, Some(elemName.name), toByteArray(elemName.text), XMLWriter.byteArrayXMLWriter(xstype))
          case "anyURI"   => DataWriter(elemName.namespace, Some(elemName.name), new java.net.URI(elemName.text), XMLWriter.XSAnyURIXMLWriter)
          case "QName"    => DataWriter(elemName.namespace, Some(elemName.name), javax.xml.namespace.QName.valueOf(elemName.text), XMLWriter.qnameXMLWriter(xstype))
          case "NOTATION" => DataWriter(elemName.namespace, Some(elemName.name), javax.xml.namespace.QName.valueOf(elemName.text), XMLWriter.qnameXMLWriter(xstype))
          case "duration" => DataWriter(elemName.namespace, Some(elemName.name), Helper.toDuration(elemName.text), XMLWriter.XSDurationXMLWriter)
          case "dateTime" => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "time"     => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "gYearMonth" => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "gYear"    => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "gMonthDay"  => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "gDay"     => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          case "gMonth"   => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLWriter.calendarXMLWriter(xstype))
          
          case _          => DataWriter(elemName.namespace, Some(elemName.name), elemName.node, __NodeSeqXMLWriter)
        }      
      case _ => DataWriter(elemName.namespace, Some(elemName.name), elemName.node, __NodeSeqXMLWriter)
    }
  
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
}

object ElemName {
  implicit def toNode(elemName: ElemName): scala.xml.Node = elemName.node
}

trait AnyElemNameParser extends scala.util.parsing.combinator.Parsers {
  import XMLWriter._
  
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
  def isMixed: Boolean = false
  
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

object XMLCalendar {
  def apply(value: String): XMLGregorianCalendar = Helper.toCalendar(value)
  def unapply(value: XMLGregorianCalendar): Option[String] = Some(value.toXMLFormat)
}

object Helper {
  val XML_SCHEMA_URI = "http://www.w3.org/2001/XMLSchema"
  val XSI_URL = "http://www.w3.org/2001/XMLSchema-instance"
  val XSI_PREFIX = "xsi"

  def toCalendar(value: String): XMLGregorianCalendar = {
    import javax.xml.datatype._
    val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()
    typeFactory.newXMLGregorianCalendar(value)
  }
  
  def toCalendar(value: java.util.GregorianCalendar): XMLGregorianCalendar = {
    import javax.xml.datatype._
    import java.util.{GregorianCalendar, Calendar => JCalendar}
    
    val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()
    val xmlGregorian = typeFactory.newXMLGregorianCalendar()
    if (value.getTimeZone != null) {
      xmlGregorian.setTimezone(value.getTimeZone.getRawOffset / 60000)
    }
        
    if (value.isSet(JCalendar.YEAR)) xmlGregorian.setYear(if (value.get(JCalendar.ERA) == GregorianCalendar.AD) value.get(JCalendar.YEAR) else -value.get(JCalendar.YEAR))
    if (value.isSet(JCalendar.MONTH)) xmlGregorian.setMonth(value.get(JCalendar.MONTH) - JCalendar.JANUARY + DatatypeConstants.JANUARY)
    if (value.isSet(JCalendar.DAY_OF_MONTH)) xmlGregorian.setDay(value.get(JCalendar.DAY_OF_MONTH))
    if (value.isSet(JCalendar.HOUR_OF_DAY)) xmlGregorian.setHour(value.get(JCalendar.HOUR_OF_DAY))
    if (value.isSet(JCalendar.MINUTE)) xmlGregorian.setMinute(value.get(JCalendar.MINUTE))
    if (value.isSet(JCalendar.SECOND)) xmlGregorian.setSecond(value.get(JCalendar.SECOND))
    if (value.isSet(JCalendar.MILLISECOND) && value.get(JCalendar.MILLISECOND) > 0) xmlGregorian.setFractionalSecond(new java.math.BigDecimal(value.get(JCalendar.MILLISECOND)))
    
    xmlGregorian
  }
  
  def toString(value: Array[Byte]) =
    (new sun.misc.BASE64Encoder()).encodeBuffer(value)
  
  def toDuration(value: String) = {
    val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()
    typeFactory.newDuration(value)
  }
  
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
  
    
  def toXML(__obj: String, __xstype: Option[String], __namespace: Option[String], __elementLabel: Option[String],
       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {
    __elementLabel map { label =>
      scala.xml.Elem(getPrefix(__namespace, __scope).orNull, label,
        __xstype map { x => 
          new scala.xml.PrefixedAttribute(__scope.getPrefix(XSI_URL), "type", x, scala.xml.Null)
        } getOrElse { scala.xml.Null },
        __scope, scala.xml.Text(__obj.toString))
    } getOrElse { scala.xml.Text(__obj) }
  }
  
  def stringToXML(__obj: String, __xstype: Option[String], __namespace: Option[String], __elementLabel: Option[String],
       __scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {
    __elementLabel map { label =>
      scala.xml.Elem(getPrefix(__namespace, __scope).orNull, label,
        __xstype map { x => 
          new scala.xml.PrefixedAttribute(__scope.getPrefix(XSI_URL), "type", x, scala.xml.Null)
        } getOrElse { scala.xml.Null },
        __scope, scala.xml.Text(__obj.toString))
    } getOrElse { scala.xml.Text(__obj) }
  }
}
