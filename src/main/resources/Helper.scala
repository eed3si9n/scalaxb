package scalaxb

import scala.xml.{Node, NodeSeq, NamespaceBinding, Elem}
import javax.xml.datatype.{XMLGregorianCalendar}

object Scalaxb {
  def fromXML[A](seq: NodeSeq)(implicit format: XMLFormat[A]): A = format.readsXML(seq)
  def toXML[A](__obj: A, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding, __typeAttribute: Boolean = false)(implicit format: CanWriteXML[A]): NodeSeq =
    format.writesXML(__obj, __namespace, __elementLabel, __scope, __typeAttribute)
}

trait XMLFormat[A] extends CanWriteXML[A] with CanReadXML[A]

trait CanReadXML[A] {
  def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, A]
  
  def readsXML(seq: scala.xml.NodeSeq): A = readsXMLEither(seq) match {
    case Right(a) => a
    case Left(a) => error(a)
  }
}

trait CanWriteXML[A] {
  def writesXML(__obj: A, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding, __typeAttribute: Boolean): NodeSeq
}

object XMLFormat {
  implicit object __NodeXMLWriter extends CanWriteXML[Node] {
    def writesXML(__obj: Node, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding, __typeAttribute: Boolean): NodeSeq = __obj
  }

  implicit object __NodeSeqXMLWriter extends CanWriteXML[NodeSeq] {
    def writesXML(__obj: NodeSeq, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding, __typeAttribute: Boolean): NodeSeq = __obj
  }
  
  implicit object __ElemXMLWriter extends CanWriteXML[Elem] {
    def writesXML(__obj: Elem, __namespace: Option[String], __elementLabel: Option[String],
      __scope: NamespaceBinding, __typeAttribute: Boolean): NodeSeq = __obj
  }  

  implicit object __StringXMLWriter extends CanWriteXML[String] {
    def writesXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, None, __namespace, __elementLabel, __scope)
  }
  
  object XSStringXMLWriter extends CanWriteXML[String] {
    def writesXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, Some("string"), __namespace, __elementLabel, __scope)
  }
  
  def stringXMLWriter(xstype: String) = new CanWriteXML[String] {
    def writesXML(__obj: String, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj, Some(xstype), __namespace, __elementLabel, __scope)
  }

  implicit object __IntXMLWriter extends CanWriteXML[Int] {
    def writesXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSIntXMLWriter extends CanWriteXML[Int] {
    def writesXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("int"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteXMLWriter extends CanWriteXML[Byte] {
    def writesXML(__obj: Byte, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSByteXMLWriter extends CanWriteXML[Byte] {
    def writesXML(__obj: Byte, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("byte"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ShortXMLWriter extends CanWriteXML[Short] {
    def writesXML(__obj: Short, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSShortXMLWriter extends CanWriteXML[Short] {
    def writesXML(__obj: Short, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("short"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __LongXMLWriter extends CanWriteXML[Long] {
    def writesXML(__obj: Long, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def longXMLWriter(xstype: String) = new CanWriteXML[Long] {
    def writesXML(__obj: Long, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigDecimalXMLWriter extends CanWriteXML[BigDecimal] {
    def writesXML(__obj: BigDecimal, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDecimalXMLWriter extends CanWriteXML[BigDecimal] {
    def writesXML(__obj: BigDecimal, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("decimal"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BigIntXMLWriter extends CanWriteXML[BigInt] {
    def writesXML(__obj: BigInt, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def bigIntXMLWriter(xstype: String) = new CanWriteXML[BigInt] {
    def writesXML(__obj: BigInt, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  def intXMLWriter(xstype: String) = new CanWriteXML[Int] {
    def writesXML(__obj: Int, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __FloatXMLWriter extends CanWriteXML[Float] {
    def writesXML(__obj: Float, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSFloatXMLWriter extends CanWriteXML[Float] {
    def writesXML(__obj: Float, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("float"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __DoubleXMLWriter extends CanWriteXML[Double] {
    def writesXML(__obj: Double, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDoubleXMLWriter extends CanWriteXML[Double] {
    def writesXML(__obj: Double, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("double"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __BooleanXMLWriter extends CanWriteXML[Boolean] {
    def writesXML(__obj: Boolean, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSBooleanXMLWriter extends CanWriteXML[Boolean] {
    def writesXML(__obj: Boolean, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("boolean"), __namespace, __elementLabel, __scope)
  }
  
  implicit object __DurationXMLWriter extends CanWriteXML[javax.xml.datatype.Duration] {
    def writesXML(__obj: javax.xml.datatype.Duration, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  object XSDurationXMLWriter extends CanWriteXML[javax.xml.datatype.Duration] {
    def writesXML(__obj: javax.xml.datatype.Duration, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("duration"), __namespace, __elementLabel, __scope)
  }

  implicit object __CalendarXMLWriter extends CanWriteXML[XMLGregorianCalendar] {
    def writesXML(__obj: XMLGregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toXMLFormat, None, __namespace, __elementLabel, __scope)
  }
  
  def calendarXMLWriter(xstype: String) = new CanWriteXML[XMLGregorianCalendar] {
    def writesXML(__obj: XMLGregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toXMLFormat, Some(xstype), __namespace, __elementLabel, __scope)
  }
    
  implicit object __GregorianCalendarXMLWriter extends CanWriteXML[java.util.GregorianCalendar] {
    def writesXML(__obj: java.util.GregorianCalendar, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toCalendar(__obj).toXMLFormat, None, __namespace, __elementLabel, __scope)
  }
    
  implicit object __QNameXMLWriter extends CanWriteXML[javax.xml.namespace.QName] {
    def writesXML(__obj: javax.xml.namespace.QName, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  }
  
  def qnameXMLWriter(xstype: String) = new CanWriteXML[javax.xml.namespace.QName] {
    def writesXML(__obj: javax.xml.namespace.QName, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __StringArrayXMLWriter extends CanWriteXML[Array[String]] {
    def writesXML(__obj: Array[String], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.mkString(" "), None, __namespace, __elementLabel, __scope)
  }
  
  def stringArrayXMLWriter(xstype: String) = new CanWriteXML[Array[String]] {
    def writesXML(__obj: Array[String], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.mkString(" "), Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __ByteArrayXMLWriter extends CanWriteXML[Array[Byte]] {
    def writesXML(__obj: Array[Byte], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), None, __namespace, __elementLabel, __scope)
  }
  
  def byteArrayXMLWriter(xstype: String) = new CanWriteXML[Array[Byte]] {
    def writesXML(__obj: Array[Byte], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), Some(xstype), __namespace, __elementLabel, __scope)
  }
  
  implicit object __HexBinaryXMLWriter extends CanWriteXML[HexBinary] {
    def writesXML(__obj: HexBinary, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), None, __namespace, __elementLabel, __scope)
  }
  
  object XSHexBinaryXMLWriter extends CanWriteXML[HexBinary] {
    def writesXML(__obj: HexBinary, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(__obj), None, __namespace, __elementLabel, __scope)
  }
  
  implicit object __URIXMLWriter extends CanWriteXML[java.net.URI] {
    def writesXML(__obj: java.net.URI, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("hexBinary"), __namespace, __elementLabel, __scope)
  }
  
  object XSAnyURIXMLWriter extends CanWriteXML[java.net.URI] {
    def writesXML(__obj: java.net.URI, __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(__obj.toString, Some("anyURI"), __namespace, __elementLabel, __scope)
  }
  
  implicit def seqXMLWriter[A: CanWriteXML]: CanWriteXML[Seq[A]] = new CanWriteXML[Seq[A]] {
    def writesXML(__obj: Seq[A], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML((__obj map { x => Scalaxb.toXML(x, __namespace, __elementLabel, __scope, __typeAttribute).text }).mkString(" "),
        None, __namespace, __elementLabel, __scope)       
  }
  
  implicit def dataRecordXMLWriter[A]: CanWriteXML[DataRecord[A]] = new CanWriteXML[DataRecord[A]] {
    def writesXML(__obj: DataRecord[A], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      DataRecord.toXML(__obj, __namespace, __elementLabel, __scope, __typeAttribute)   
  }
  
  implicit object __NoneXMLWriter extends CanWriteXML[Option[Nothing]] {
    def writesXML(__obj: Option[Nothing], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.nilElem(__namespace, __elementLabel.get, __scope)    
  }
  
  implicit def someXMLWriter[A: CanWriteXML]: CanWriteXML[Some[A]] = new CanWriteXML[Some[A]] {
    def writesXML(__obj: Some[A], __namespace: Option[String], __elementLabel: Option[String],
        __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
      Scalaxb.toXML[A](__obj.get, __namespace, __elementLabel, __scope, __typeAttribute)
  } 
  
  // implicit object AnyXMLWriter extends CanWriteXML[Any] {
  //   def writesXML(__obj: Any, __namespace: Option[String], __elementLabel: Option[String],
  //       __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
  //     Helper.stringToXML(__obj.toString, None, __namespace, __elementLabel, __scope)
  // } 
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
    writer: CanWriteXML[_]) extends DataRecord[A]
  import Helper._
  
  def apply(namespace: Option[String], key: Option[String], value: Option[Nothing]): DataRecord[Option[Nothing]] =
    DataWriter(namespace, key, value, XMLFormat.__NoneXMLWriter)
  
  def apply[A:CanWriteXML](namespace: Option[String], key: Option[String], value: A): DataRecord[A] =
    DataWriter(namespace, key, value, implicitly[CanWriteXML[A]])
  
  def apply[A:CanWriteXML](value: A): DataRecord[A] =
    apply(None, None, value)
  
  def apply(elemName: ElemName): DataRecord[Any] =
    if (elemName.name == "") DataWriter(None, None, elemName.text, XMLFormat.__StringArrayXMLWriter)
    else instanceType(elemName.node) match {
      case (Some(XML_SCHEMA_URI), xstype)   =>
        xstype match {
          case "int"      => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.XSIntXMLWriter)
          case "byte"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toByte, XMLFormat.XSByteXMLWriter)
          case "short"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toShort, XMLFormat.XSShortXMLWriter)
          case "long"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toLong, XMLFormat.longXMLWriter(xstype))
          case "float"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toFloat, XMLFormat.XSFloatXMLWriter)
          case "double"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toDouble, XMLFormat.XSDoubleXMLWriter)
          case "integer"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "nonPositiveInteger" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "negativeInteger"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "nonNegativeInteger" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "positiveInteger"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "unsignedLong"    => DataWriter(elemName.namespace, Some(elemName.name), BigInt(elemName.text), XMLFormat.bigIntXMLWriter(xstype))
          case "unsignedInt"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toLong, XMLFormat.longXMLWriter(xstype))
          case "unsignedShort"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "unsignedByte"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toInt, XMLFormat.intXMLWriter(xstype))
          case "decimal"  => DataWriter(elemName.namespace, Some(elemName.name), BigDecimal(elemName.text), XMLFormat.XSDecimalXMLWriter)
          case "boolean"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text.toBoolean, XMLFormat.XSBooleanXMLWriter)
          case "string"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.XSStringXMLWriter)
          case "normalizedString" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "token"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "language" => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "Name"     => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "NCName"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "NMTOKEN"  => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "NMTOKENS" => DataWriter(elemName.namespace, Some(elemName.name), elemName.splitBySpace, XMLFormat.stringArrayXMLWriter(xstype))
          case "ID"       => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "IDREF"    => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "IDREFS"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.splitBySpace, XMLFormat.stringArrayXMLWriter(xstype))
          case "ENTITY"   => DataWriter(elemName.namespace, Some(elemName.name), elemName.text, XMLFormat.stringXMLWriter(xstype))
          case "ENTITIES" => DataWriter(elemName.namespace, Some(elemName.name), elemName.splitBySpace, XMLFormat.stringArrayXMLWriter(xstype))
          case "hexBinary"    => DataWriter(elemName.namespace, Some(elemName.name), toHexBinary(elemName.text), XMLFormat.XSHexBinaryXMLWriter)
          case "base64Binary" => DataWriter(elemName.namespace, Some(elemName.name), toByteArray(elemName.text), XMLFormat.byteArrayXMLWriter(xstype))
          case "anyURI"   => DataWriter(elemName.namespace, Some(elemName.name), new java.net.URI(elemName.text), XMLFormat.XSAnyURIXMLWriter)
          case "QName"    => DataWriter(elemName.namespace, Some(elemName.name), javax.xml.namespace.QName.valueOf(elemName.text), XMLFormat.qnameXMLWriter(xstype))
          case "NOTATION" => DataWriter(elemName.namespace, Some(elemName.name), javax.xml.namespace.QName.valueOf(elemName.text), XMLFormat.qnameXMLWriter(xstype))
          case "duration" => DataWriter(elemName.namespace, Some(elemName.name), Helper.toDuration(elemName.text), XMLFormat.XSDurationXMLWriter)
          case "dateTime" => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "time"     => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "gYearMonth" => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "gYear"    => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "gMonthDay"  => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "gDay"     => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          case "gMonth"   => DataWriter(elemName.namespace, Some(elemName.name), XMLCalendar(elemName.text), XMLFormat.calendarXMLWriter(xstype))
          
          case _          => DataWriter(elemName.namespace, Some(elemName.name), elemName.node, XMLFormat.__NodeSeqXMLWriter)
        }      
      case _ => DataWriter(elemName.namespace, Some(elemName.name), elemName.node, XMLFormat.__NodeSeqXMLWriter)
    }
  
  def unapply[A](record: DataRecord[A]): Option[(Option[String], Option[String], A)] =
    Some(record.namespace, record.key, record.value)
  
  def toXML[A](__obj: DataRecord[A], __namespace: Option[String], __elementLabel: Option[String],
      __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq = __obj match {
    case w: DataWriter[_] => w.writer.asInstanceOf[CanWriteXML[A]].writesXML(__obj.value, __namespace, __elementLabel, __scope, __typeAttribute)
    case _ => error("unknown DataRecord.")
  }
}

case class ElemName(namespace: Option[String], name: String) {
  var node: scala.xml.Node = _
  def text = node.text
  def nil = Helper.isNil(node)
  def splitBySpace = Helper.splitBySpace(text)
}

object ElemName {
  implicit def toNodeSeq(elemName: ElemName): scala.xml.NodeSeq = elemName.node
}

trait AnyElemNameParser extends scala.util.parsing.combinator.Parsers {
  import XMLFormat._
  
  type Elem = ElemName
  
  def any: Parser[ElemName] = 
    accept("any", { case x: ElemName if x.name != "" => x })  
  
  def optTextRecord = opt(text ^^ (x => DataRecord(x.node.text)))
  
  def text: Parser[ElemName] =
    accept("text", { case x: ElemName if x.name == "" => x })
}

trait CanWriteChildNodes[A] extends CanWriteXML[A] {
  def typeName: Option[String] = None
  def writesAttribute(__obj: A, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData = scala.xml.Null
  def writesChildNodes(__obj: A, __scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node]
  
  def writesXML(__obj: A, __namespace: Option[String], __elementLabel: Option[String], 
      __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq =
    scala.xml.Elem(Helper.getPrefix(__namespace, __scope).orNull,
      __elementLabel getOrElse { error("missing element label.") },
      if (__typeAttribute && typeName.isDefined &&
          __scope.getPrefix(Helper.XSI_URL) != null) scala.xml.Attribute(__scope.getPrefix(Helper.XSI_URL), "type", typeName.get,
        writesAttribute(__obj, __scope))
      else writesAttribute(__obj, __scope),
      __scope,
      writesChildNodes(__obj, __scope): _*)  
}

trait ElemNameParser[A] extends AnyElemNameParser with XMLFormat[A] with CanWriteChildNodes[A] {
  def readsXMLEither(seq: scala.xml.NodeSeq): Either[String, A] = seq match {
    case node: scala.xml.Node =>
      parse(parser(node), node.child) match {
        case x: Success[_] => Right(x.get)
        case x: Failure => Left(x.msg)
        case x: Error => Left(x.msg)
      }
    case _ => Left("seq must be scala.xml.Node")
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

class HexBinary(_length: Int) extends scala.collection.IndexedSeq[Byte] {
  private var array = new Array[Byte](_length)
  
  def length = array.length
  def apply(idx: Int): Byte = array(idx)
  override def toString: String = Helper.toString(this)
}

object HexBinary {
  def apply(xs: Byte*) = {
    val value = new HexBinary(xs.length)
    var i = 0
    for (x <- xs.iterator) { value.array(i) = x; i += 1 }
    value
  }
  
  def unapplySeq[Byte](x: HexBinary) = Some(x.toIndexedSeq)
}

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
  
  def toString(value: Array[Byte]): String =
    (new sun.misc.BASE64Encoder()).encodeBuffer(value).stripLineEnd
  
  def toString(value: HexBinary): String =
    (value map { x => ("0" + Integer.toHexString(x.toInt)).takeRight(2) }).mkString.toUpperCase
  
  def toDuration(value: String) = {
    val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()
    typeFactory.newDuration(value)
  }
  
  def toByteArray(value: String) =
    (new sun.misc.BASE64Decoder()).decodeBuffer(value)
  
  def toHexBinary(value: String) = {
    val array = new Array[Byte](value.length / 2)
    for (i <- 0 to array.length - 1) {
      array(i) = Integer.parseInt(value.drop(i * 2).take(2), 16).toByte
    }
    HexBinary(array: _*)
  }
  
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
      
  def splitBySpace(text: String) = text.split(' ').filter("" !=)
  
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
  
    
  def writesXML(__obj: String, __xstype: Option[String], __namespace: Option[String], __elementLabel: Option[String],
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
