package scalaxb

import scala.xml.{Node, NodeSeq, NamespaceBinding, Elem, UnprefixedAttribute, PrefixedAttribute}
import javax.xml.datatype.{XMLGregorianCalendar}

object `package` {
  def fromXML[A](seq: NodeSeq, stack: List[ElemName] = Nil)
                (implicit format: XMLFormat[A]): A = format.reads(seq, stack) match {
    case Right(a) => a
    case Left(a) => throw new ParserFailure(a)
  }

  def fromXMLEither[A](seq: NodeSeq, stack: List[ElemName] = Nil)
                      (implicit format: XMLFormat[A]): Either[String, A] = format.reads(seq, stack)

  def toXML[A](obj: A, namespace: Option[String], elementLabel: Option[String],
      scope: NamespaceBinding, typeAttribute: Boolean = false)(implicit format: CanWriteXML[A]): NodeSeq =
    format.writes(obj, namespace, elementLabel, scope, typeAttribute)
  def toXML[A](obj: A, namespace: Option[String], elementLabel: String, scope: NamespaceBinding)
      (implicit format: CanWriteXML[A]): NodeSeq =
    toXML(obj, namespace, Some(elementLabel), scope, false)
  def toXML[A](obj: A, elementLabel: String, scope: NamespaceBinding)(implicit format: CanWriteXML[A]): NodeSeq =
    toXML(obj, None, Some(elementLabel), scope, false)

  /** @param pairs - pairs of (prefix, namespace URI)
   */
  def toScope(pairs: (Option[String], String)*): NamespaceBinding =
    pairs.reverse.foldLeft[NamespaceBinding](scala.xml.TopScope) { (scope, pair) =>
      scala.xml.NamespaceBinding(pair._1.getOrElse{null}, pair._2, scope) }
}

trait XMLFormat[A] extends CanWriteXML[A] with CanReadXML[A]

trait CanReadXML[A] {
  def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, A]
}

trait CanWriteXML[A] {
  def writes(obj: A, namespace: Option[String], elementLabel: Option[String],
      scope: NamespaceBinding, typeAttribute: Boolean): NodeSeq
}

trait DefaultXMLStandardTypes extends XMLStandardTypes {
  override def __buildNodeXMLFormat: XMLFormat[Node] = new XMLFormat[Node] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Node] = seq match {
      case node: Node => Right(node)
      case _ => Left("scala.xml.Node is required.")
    }

    def writes(obj: Node, namespace: Option[String], elementLabel: Option[String],
      scope: NamespaceBinding, typeAttribute: Boolean): NodeSeq = obj
  }

  override def __buildNodeSeqXMLFormat: XMLFormat[NodeSeq] = new XMLFormat[NodeSeq] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, NodeSeq] = Right(seq)

    def writes(obj: NodeSeq, namespace: Option[String], elementLabel: Option[String],
      scope: NamespaceBinding, typeAttribute: Boolean): NodeSeq = obj
  }

  override def __buildElemXMLFormat: XMLFormat[Elem] = new XMLFormat[Elem] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Elem] = seq match {
      case elem: Elem => Right(elem)
      case _ => Left("scala.xml.Elem is required.")
    }

    def writes(obj: Elem, namespace: Option[String], elementLabel: Option[String],
      scope: NamespaceBinding, typeAttribute: Boolean): NodeSeq = obj
  }

  override def __buildStringXMLFormat: XMLFormat[String] = new XMLFormat[String] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, String] = Right(seq.text)

    def writes(obj: String, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj, namespace, elementLabel, scope)
  }

  override def __buildIntXMLFormat: XMLFormat[Int] = new XMLFormat[Int] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Int] = try {
      Right(seq.text.toInt) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Int, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildByteXMLFormat: XMLFormat[Byte] = new XMLFormat[Byte] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Byte] = try {
      Right(seq.text.toByte) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Byte, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildShortXMLFormat: XMLFormat[Short] = new XMLFormat[Short] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Short] = try {
      Right(seq.text.toShort) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Short, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildLongXMLFormat: XMLFormat[Long] = new XMLFormat[Long] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Long] = try {
      Right(seq.text.toLong) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Long, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildBigDecimalXMLFormat: XMLFormat[BigDecimal] = new XMLFormat[BigDecimal] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, BigDecimal] = try {
      Right(BigDecimal(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: BigDecimal, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildBigIntXMLFormat: XMLFormat[BigInt] = new XMLFormat[BigInt] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, BigInt] = try {
      Right(BigInt(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: BigInt, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildFloatXMLFormat: XMLFormat[Float] = new XMLFormat[Float] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Float] = try {
      Right(seq.text.toFloat) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Float, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildDoubleXMLFormat: XMLFormat[Double] = new XMLFormat[Double] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Double] = try {
      Right(seq.text.toDouble) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Double, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildBooleanXMLFormat: XMLFormat[Boolean] = new XMLFormat[Boolean] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Boolean] = try {
      Right(seq.text.toBoolean) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Boolean, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildDurationXMLFormat: XMLFormat[javax.xml.datatype.Duration] = new XMLFormat[javax.xml.datatype.Duration] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, javax.xml.datatype.Duration] =
      try { Right(Helper.toDuration(seq.text)) }
      catch { case e: Exception => Left(e.toString) }

    def writes(obj: javax.xml.datatype.Duration, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildCalendarXMLFormat: XMLFormat[XMLGregorianCalendar] = new XMLFormat[XMLGregorianCalendar] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, XMLGregorianCalendar] = try {
      Right(XMLCalendar(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: XMLGregorianCalendar, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toXMLFormat, namespace, elementLabel, scope)
  }

  override def __buildGregorianCalendarXMLWriter: CanWriteXML[java.util.GregorianCalendar] = new CanWriteXML[java.util.GregorianCalendar] {
    def writes(obj: java.util.GregorianCalendar, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toCalendar(obj).toXMLFormat, namespace, elementLabel, scope)
  }

  override def __buildQNameXMLFormat: XMLFormat[javax.xml.namespace.QName] = new  XMLFormat[javax.xml.namespace.QName] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, javax.xml.namespace.QName] = try {
      Right(javax.xml.namespace.QName.valueOf(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: javax.xml.namespace.QName, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildStringArrayXMLFormat: XMLFormat[Array[String]] = new XMLFormat[Array[String]] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Array[String]] = try {
      Right(Helper.splitBySpace(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Array[String], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.mkString(" "), namespace, elementLabel, scope)
  }

  override def __buildByteArrayXMLFormat: XMLFormat[Array[Byte]] = new XMLFormat[Array[Byte]] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Array[Byte]] = try {
      Right(Helper.toByteArray(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Array[Byte], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(obj), namespace, elementLabel, scope)
  }

  override def __buildHexBinaryXMLFormat: XMLFormat[HexBinary] = new XMLFormat[HexBinary] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, HexBinary] = try {
      Right(Helper.toHexBinary(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: HexBinary, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(Helper.toString(obj), namespace, elementLabel, scope)
  }

  override def __buildURIXMLFormat: XMLFormat[java.net.URI] = new XMLFormat[java.net.URI] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, java.net.URI] = try {
      Right(Helper.toURI(seq.text)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: java.net.URI, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML(obj.toString, namespace, elementLabel, scope)
  }

  override def __buildNoneXMLWriter: CanWriteXML[None.type] = new  CanWriteXML[None.type] {
    def writes(obj: None.type, namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.nilElem(namespace, elementLabel.get, scope)
  }

  override def __buildDataRecordAnyXMLFormat: XMLFormat[DataRecord[Any]] = new XMLFormat[DataRecord[Any]] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, DataRecord[Any]] = try {
      Right(DataRecord.fromAny(seq)) } catch { case e: Exception => Left(e.toString) }

    def writes(obj: DataRecord[Any], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      DataRecord.toXML(obj, namespace, elementLabel, scope, typeAttribute)
  }

  override def __buildDataRecordOptionAnyXMLFormat: XMLFormat[DataRecord[Option[Any]]] =
    new XMLFormat[DataRecord[Option[Any]]] {
      def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, DataRecord[Option[Any]]] = try {
        Right(DataRecord.fromNillableAny(seq)) } catch { case e: Exception => Left(e.toString) }

      def writes(obj: DataRecord[Option[Any]], namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        DataRecord.toXML(obj, namespace, elementLabel, scope, typeAttribute)
    }

  override def __buildDataRecordMapWriter: CanWriteXML[Map[String, scalaxb.DataRecord[Any]]] =
    new CanWriteXML[Map[String, scalaxb.DataRecord[Any]]] {
      def writes(obj: Map[String, scalaxb.DataRecord[Any]], namespace: Option[String], elementLabel: Option[String],
          scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
        obj.valuesIterator.toList flatMap { x =>
          scalaxb.toXML[DataRecord[Any]](x, x.namespace, x.key, scope, typeAttribute)
        }
    }
}

trait XMLStandardTypes {
  implicit lazy val __NodeXMLFormat: XMLFormat[Node] = __buildNodeXMLFormat
  def __buildNodeXMLFormat: XMLFormat[Node]

  implicit lazy val __NodeSeqXMLFormat: XMLFormat[NodeSeq] = __buildNodeSeqXMLFormat
  def __buildNodeSeqXMLFormat: XMLFormat[NodeSeq]

  implicit lazy val __ElemXMLFormat: XMLFormat[Elem] = __buildElemXMLFormat
  def __buildElemXMLFormat: XMLFormat[Elem]

  implicit lazy val __StringXMLFormat: XMLFormat[String] = __buildStringXMLFormat
  def __buildStringXMLFormat: XMLFormat[String]

  implicit lazy val __IntXMLFormat: XMLFormat[Int] = __buildIntXMLFormat
  def __buildIntXMLFormat: XMLFormat[Int]

  implicit lazy val __ByteXMLFormat: XMLFormat[Byte] = __buildByteXMLFormat
  def __buildByteXMLFormat: XMLFormat[Byte]

  implicit lazy val __ShortXMLFormat: XMLFormat[Short] = __buildShortXMLFormat
  def __buildShortXMLFormat: XMLFormat[Short]

  implicit lazy val __LongXMLFormat: XMLFormat[Long] = __buildLongXMLFormat
  def __buildLongXMLFormat: XMLFormat[Long]

  implicit lazy val __BigDecimalXMLFormat: XMLFormat[BigDecimal] = __buildBigDecimalXMLFormat
  def __buildBigDecimalXMLFormat: XMLFormat[BigDecimal]

  implicit lazy val __BigIntXMLFormat: XMLFormat[BigInt] = __buildBigIntXMLFormat
  def __buildBigIntXMLFormat: XMLFormat[BigInt]

  implicit lazy val __FloatXMLFormat: XMLFormat[Float] = __buildFloatXMLFormat
  def __buildFloatXMLFormat: XMLFormat[Float]

  implicit lazy val __DoubleXMLFormat: XMLFormat[Double] = __buildDoubleXMLFormat
  def __buildDoubleXMLFormat: XMLFormat[Double]

  implicit lazy val __BooleanXMLFormat: XMLFormat[Boolean] = __buildBooleanXMLFormat
  def __buildBooleanXMLFormat: XMLFormat[Boolean]

  implicit lazy val __DurationXMLFormat: XMLFormat[javax.xml.datatype.Duration] = __buildDurationXMLFormat
  def __buildDurationXMLFormat: XMLFormat[javax.xml.datatype.Duration]

  implicit lazy val __CalendarXMLFormat: XMLFormat[XMLGregorianCalendar] = __buildCalendarXMLFormat
  def __buildCalendarXMLFormat: XMLFormat[XMLGregorianCalendar]

  implicit lazy val __GregorianCalendarXMLWriter: CanWriteXML[java.util.GregorianCalendar] = __buildGregorianCalendarXMLWriter
  def __buildGregorianCalendarXMLWriter: CanWriteXML[java.util.GregorianCalendar]

  implicit lazy val __QNameXMLFormat: XMLFormat[javax.xml.namespace.QName] = __buildQNameXMLFormat
  def __buildQNameXMLFormat: XMLFormat[javax.xml.namespace.QName]

  implicit lazy val __StringArrayXMLFormat: XMLFormat[Array[String]] = __buildStringArrayXMLFormat
  def __buildStringArrayXMLFormat: XMLFormat[Array[String]]

  implicit lazy val __ByteArrayXMLFormat: XMLFormat[Array[Byte]] = __buildByteArrayXMLFormat
  def __buildByteArrayXMLFormat: XMLFormat[Array[Byte]]

  implicit lazy val __HexBinaryXMLFormat: XMLFormat[HexBinary] = __buildHexBinaryXMLFormat
  def __buildHexBinaryXMLFormat: XMLFormat[HexBinary]

  implicit lazy val __URIXMLFormat: XMLFormat[java.net.URI] = __buildURIXMLFormat
  def __buildURIXMLFormat: XMLFormat[java.net.URI]

  implicit def seqXMLFormat[A: XMLFormat]: XMLFormat[Seq[A]] = new XMLFormat[Seq[A]] {
    def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, Seq[A]] = try {
      val xs = Helper.splitBySpace(seq.text).toSeq
      Right(xs map { x => fromXML[A](scala.xml.Text(x), stack) })
    } catch { case e: Exception => Left(e.toString) }

    def writes(obj: Seq[A], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      Helper.stringToXML((obj map { x => scalaxb.toXML(x, namespace, elementLabel, scope, typeAttribute).text }).mkString(" "),
        namespace, elementLabel, scope)
  }

  implicit def dataRecordXMLWriter[A]: CanWriteXML[DataRecord[A]] = new CanWriteXML[DataRecord[A]] {
    def writes(obj: DataRecord[A], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      DataRecord.toXML(obj, namespace, elementLabel, scope, typeAttribute)
  }

  implicit lazy val __NoneXMLWriter: CanWriteXML[None.type] = __buildNoneXMLWriter
  def __buildNoneXMLWriter: CanWriteXML[None.type]

  implicit def someXMLWriter[A: CanWriteXML]: CanWriteXML[Some[A]] = new CanWriteXML[Some[A]] {
    def writes(obj: Some[A], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
      scalaxb.toXML[A](obj.get, namespace, elementLabel, scope, typeAttribute)
  }

  implicit def optionXMLWriter[A: CanWriteXML]: CanWriteXML[Option[A]] = new CanWriteXML[Option[A]] {
    def writes(obj: Option[A], namespace: Option[String], elementLabel: Option[String],
        scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq = obj match {
      case Some(x) => scalaxb.toXML[A](x, namespace, elementLabel, scope, typeAttribute)
      case None    => Helper.nilElem(namespace, elementLabel.get, scope)
    }
  }

  implicit lazy val __DataRecordAnyXMLFormat: XMLFormat[DataRecord[Any]] = __buildDataRecordAnyXMLFormat
  def __buildDataRecordAnyXMLFormat: XMLFormat[DataRecord[Any]]

  implicit lazy val __DataRecordOptionAnyXMLFormat: XMLFormat[DataRecord[Option[Any]]] = __buildDataRecordOptionAnyXMLFormat
  def __buildDataRecordOptionAnyXMLFormat: XMLFormat[DataRecord[Option[Any]]]

  implicit lazy val __DataRecordMapWriter: CanWriteXML[Map[String, scalaxb.DataRecord[Any]]] = __buildDataRecordMapWriter
  def __buildDataRecordMapWriter: CanWriteXML[Map[String, scalaxb.DataRecord[Any]]]
}

trait DataRecord[+A] {
  val namespace: Option[String]
  val key: Option[String]
  val value: A

  def as[B] = value.asInstanceOf[B]

  override def toString: String = {
    "DataRecord(" +
    ((namespace, key, value) match {
      case (None, Some(k), _) => k + "," + value.toString
      case (Some(n), Some(k), _) => "{" + n + "}" + k + "," + value.toString
      case _ => value.toString
    }) + ")"
  }
}

object DataRecord extends DefaultXMLStandardTypes {
  private case class DataWriter[+A](
    namespace: Option[String],
    key: Option[String],
    xstypeNamespace: Option[String],
    xstypeName: Option[String],
    value: A,
    writer: CanWriteXML[_]) extends DataRecord[A]
  import Helper._

  // this is for nil element.
  def apply(namespace: Option[String], key: Option[String], value: None.type): DataRecord[Option[Nothing]] =
    DataWriter(namespace, key, None, None, value, __NoneXMLWriter)

  // this is for choice option: DataRecord(x.namespace, Some(x.name), fromXML[Address](x))
  def apply[A:CanWriteXML](namespace: Option[String], key: Option[String], value: A): DataRecord[A] =
    DataWriter(namespace, key, None, None, value, implicitly[CanWriteXML[A]])

  def apply[A:CanWriteXML](node: Node, value: A): DataRecord[A] = node match {
    case elem: Elem =>
      val ns = Option[String](elem.scope.getURI(elem.prefix))
      val key = Some(elem.label)
      DataRecord(ns, key, value)
    case _ => DataRecord(value)
  }

  def apply[A:CanWriteXML](node: Node, parent: Node, value: A): DataRecord[A] = node match {
    case elem: Elem => DataRecord(node, value)
    case attr: UnprefixedAttribute =>
      val key = Some(attr.key)
      DataRecord(None, key, value)
    case attr: PrefixedAttribute =>
      val ns = Option[String](attr.getNamespace(node))
      val key = Some(attr.key)
      DataRecord(ns, key, value)
    case _ => DataRecord(value)
  }

  def apply[A:CanWriteXML](value: A): DataRecord[A] =
    apply(None, None, value)

  def apply[A:CanWriteXML](namespace: Option[String], key: Option[String],
      xstypeNamespace: Option[String], xstypeName: Option[String], value: A): DataRecord[A] =
    DataWriter(namespace, key, xstypeNamespace, xstypeName, value, implicitly[CanWriteXML[A]])

  // this is for any.
  def apply(elemName: ElemName): DataRecord[Any] = fromAny(elemName.node)

  def fromAny(seq: NodeSeq): DataRecord[Any] = {
    seq match {
      case elem: Elem => fromAny(elem)
      case _ => DataRecord(None, None, None, None, seq.text)
    }
  }

  def fromAny(elem: Elem): DataRecord[Any] = {
    val ns = Option[String](elem.scope.getURI(elem.prefix))
    val key = Some(elem.label)
    val XS = Some(XML_SCHEMA_URI)

    instanceType(elem) match {
      case (XS, xstype)   =>
        xstype match {
          case Some("int")                => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("byte")               => DataRecord(ns, key, XS, xstype, fromXML[Byte](elem, Nil))
          case Some("short")              => DataRecord(ns, key, XS, xstype, fromXML[Short](elem, Nil))
          case Some("long")               => DataRecord(ns, key, XS, xstype, fromXML[Long](elem, Nil))
          case Some("float")              => DataRecord(ns, key, XS, xstype, fromXML[Float](elem, Nil))
          case Some("double")             => DataRecord(ns, key, XS, xstype, fromXML[Double](elem, Nil))
          case Some("integer")            => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("nonPositiveInteger") => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("negativeInteger")    => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("nonNegativeInteger") => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("positiveInteger")    => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("unsignedLong")       => DataRecord(ns, key, XS, xstype, fromXML[BigInt](elem, Nil))
          case Some("unsignedInt")        => DataRecord(ns, key, XS, xstype, fromXML[Long](elem, Nil))
          case Some("unsignedShort")      => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("unsignedByte")       => DataRecord(ns, key, XS, xstype, fromXML[Int](elem, Nil))
          case Some("decimal")            => DataRecord(ns, key, XS, xstype, fromXML[BigDecimal](elem, Nil))
          case Some("boolean")            => DataRecord(ns, key, XS, xstype, fromXML[Boolean](elem, Nil))
          case Some("string")             => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("normalizedString")   => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("token")              => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("language")           => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("Name")               => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("NCName")             => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("NMTOKEN")            => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("NMTOKENS")           => DataRecord(ns, key, XS, xstype, fromXML[Array[String]](elem, Nil))
          case Some("ID")                 => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("IDREF")              => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("IDREFS")             => DataRecord(ns, key, XS, xstype, fromXML[Array[String]](elem, Nil))
          case Some("ENTITY")             => DataRecord(ns, key, XS, xstype, fromXML[String](elem, Nil))
          case Some("ENTITIES")           => DataRecord(ns, key, XS, xstype, fromXML[Array[String]](elem, Nil))
          case Some("hexBinary")          => DataRecord(ns, key, XS, xstype, fromXML[HexBinary](elem, Nil))
          case Some("base64Binary")       => DataRecord(ns, key, XS, xstype, fromXML[Array[Byte]](elem, Nil))
          case Some("anyURI")             => DataRecord(ns, key, XS, xstype, fromXML[java.net.URI](elem, Nil))
          case Some("QName")              => DataRecord(ns, key, XS, xstype, fromXML[javax.xml.namespace.QName](elem, Nil))
          case Some("NOTATION")           => DataRecord(ns, key, XS, xstype, fromXML[javax.xml.namespace.QName](elem, Nil))
          case Some("duration")           => DataRecord(ns, key, XS, xstype, fromXML[javax.xml.datatype.Duration](elem, Nil))
          case Some("dateTime")           => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("time")               => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("gYearMonth")         => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("gYear")              => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("gMonthDay")          => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("gDay")               => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))
          case Some("gMonth")             => DataRecord(ns, key, XS, xstype, fromXML[XMLGregorianCalendar](elem, Nil))

          case _          => DataRecord(ns, key, XS, xstype, elem)
        }
      case _ =>
        val (xsns, xstype) = instanceType(elem)
        DataRecord(ns, key, xsns, xstype, elem)
    }
  }

  // this is for any.
  def fromNillableAny(seq: NodeSeq): DataRecord[Option[Any]] = {
    seq match {
      case elem: Elem => fromNillableAny(elem)
      case _ => DataRecord(None, None, None, None, Some(seq.text))
    }
  }

  // this is for any.
  def fromNillableAny(elem: Elem): DataRecord[Option[Any]] = {
    val ns = Option[String](elem.scope.getURI(elem.prefix))
    val key = Some(elem.label)
    val XS = Some(XML_SCHEMA_URI)

    if (isNil(elem)) DataRecord(ns, key, None)
    else instanceType(elem) match {
      case (XS, xstype)   =>
        xstype match {
          case Some("int")                => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("byte")               => DataRecord(ns, key, XS, xstype, Some(fromXML[Byte](elem, Nil)))
          case Some("short")              => DataRecord(ns, key, XS, xstype, Some(fromXML[Short](elem, Nil)))
          case Some("long")               => DataRecord(ns, key, XS, xstype, Some(fromXML[Long](elem, Nil)))
          case Some("float")              => DataRecord(ns, key, XS, xstype, Some(fromXML[Float](elem, Nil)))
          case Some("double")             => DataRecord(ns, key, XS, xstype, Some(fromXML[Double](elem, Nil)))
          case Some("integer")            => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("nonPositiveInteger") => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("negativeInteger")    => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("nonNegativeInteger") => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("positiveInteger")    => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("unsignedLong")       => DataRecord(ns, key, XS, xstype, Some(fromXML[BigInt](elem, Nil)))
          case Some("unsignedInt")        => DataRecord(ns, key, XS, xstype, Some(fromXML[Long](elem, Nil)))
          case Some("unsignedShort")      => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("unsignedByte")       => DataRecord(ns, key, XS, xstype, Some(fromXML[Int](elem, Nil)))
          case Some("decimal")            => DataRecord(ns, key, XS, xstype, Some(fromXML[BigDecimal](elem, Nil)))
          case Some("boolean")            => DataRecord(ns, key, XS, xstype, Some(fromXML[Boolean](elem, Nil)))
          case Some("string")             => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("normalizedString")   => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("token")              => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("language")           => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("Name")               => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("NCName")             => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("NMTOKEN")            => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("NMTOKENS")           => DataRecord(ns, key, XS, xstype, Some(fromXML[Array[String]](elem, Nil)))
          case Some("ID")                 => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("IDREF")              => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("IDREFS")             => DataRecord(ns, key, XS, xstype, Some(fromXML[Array[String]](elem, Nil)))
          case Some("ENTITY")             => DataRecord(ns, key, XS, xstype, Some(fromXML[String](elem, Nil)))
          case Some("ENTITIES")           => DataRecord(ns, key, XS, xstype, Some(fromXML[Array[String]](elem, Nil)))
          case Some("hexBinary")          => DataRecord(ns, key, XS, xstype, Some(fromXML[HexBinary](elem, Nil)))
          case Some("base64Binary")       => DataRecord(ns, key, XS, xstype, Some(fromXML[Array[Byte]](elem, Nil)))
          case Some("anyURI")             => DataRecord(ns, key, XS, xstype, Some(fromXML[java.net.URI](elem, Nil)))
          case Some("QName")              => DataRecord(ns, key, XS, xstype, Some(fromXML[javax.xml.namespace.QName](elem, Nil)))
          case Some("NOTATION")           => DataRecord(ns, key, XS, xstype, Some(fromXML[javax.xml.namespace.QName](elem, Nil)))
          case Some("duration")           => DataRecord(ns, key, XS, xstype, Some(fromXML[javax.xml.datatype.Duration](elem, Nil)))
          case Some("dateTime")           => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("time")               => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("gYearMonth")         => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("gYear")              => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("gMonthDay")          => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("gDay")               => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))
          case Some("gMonth")             => DataRecord(ns, key, XS, xstype, Some(fromXML[XMLGregorianCalendar](elem, Nil)))

          case _          => DataRecord(ns, key, XS, xstype, Some(elem))
        }
      case _ =>
        val (xsns, xstype) = instanceType(elem)
        DataRecord(ns, key, xsns, xstype, Some(elem))
    }
  }

  def unapply[A](record: DataRecord[A]): Option[(Option[String], Option[String], A)] =
    Some(record.namespace, record.key, record.value)

  def toXML[A](obj: DataRecord[A], namespace: Option[String], elementLabel: Option[String],
      scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq = obj match {
    case w: DataWriter[_] =>
      obj.value match {
        case seq: NodeSeq =>
          w.writer.asInstanceOf[CanWriteXML[A]].writes(obj.value, namespace, elementLabel, scope, typeAttribute)
        case _ =>
          w.writer.asInstanceOf[CanWriteXML[A]].writes(obj.value, namespace, elementLabel, scope, false) match {
            case elem: Elem if (w.xstypeNamespace.isDefined && w.xstypeName.isDefined &&
                scope.getPrefix(w.xstypeNamespace.get) != null &&
                scope.getPrefix(XSI_URL) != null) =>
              elem % new PrefixedAttribute(scope.getPrefix(XSI_URL), "type",
                scope.getPrefix(w.xstypeNamespace.get) + ":" + w.xstypeName.get, scala.xml.Null)
            case x => x
          }
      }
    case _ => error("unknown DataRecord.")
  }
}

case class ElemName(namespace: Option[String], name: String) {
  var node: scala.xml.Node = _
  def text = node.text
  def nil = Helper.isNil(node)
  def nilOption: Option[ElemName] = if (nil) None else Some(this)
  def splitBySpace = Helper.splitBySpace(text)
  override def toString = namespace match {
    case Some(x) => "{%s}%s".format(x, name)
    case _ => name
  }
}

object ElemName {
  implicit def apply(node: scala.xml.Node): ElemName = node match {
    case x: scala.xml.Elem =>
      val elemName = ElemName(Option[String](x.scope.getURI(x.prefix)), x.label)
      elemName.node = x
      elemName
    case _ =>
      val elemName = ElemName(None, "")
      elemName.node = node
      elemName
  }
}

trait AnyElemNameParser extends scala.util.parsing.combinator.Parsers {
  type Elem = ElemName

  // we need this so treat ElemName as NodeSeq for fromXML etc.
  implicit def toNodeSeq(elem: Elem): scala.xml.NodeSeq = elem.node

  def any: Parser[ElemName] =
    accept("any", { case x: ElemName if x.name != "" => x })

  def optTextRecord(implicit format: XMLFormat[String]) = opt(text ^^ (x => DataRecord(x.node.text)))

  def text: Parser[ElemName] =
    accept("text", { case x: ElemName if x.name == "" => x })
}

trait CanWriteChildNodes[A] extends CanWriteXML[A] {
  def typeName: Option[String] = None
  def writesAttribute(obj: A, scope: scala.xml.NamespaceBinding): scala.xml.MetaData = scala.xml.Null
  def writesChildNodes(obj: A, scope: scala.xml.NamespaceBinding): Seq[scala.xml.Node]

  def writes(obj: A, namespace: Option[String], elementLabel: Option[String],
      scope: scala.xml.NamespaceBinding, typeAttribute: Boolean): scala.xml.NodeSeq =
    scala.xml.Elem(Helper.getPrefix(namespace, scope).orNull,
      elementLabel getOrElse { error("missing element label.") },
      if (typeAttribute && typeName.isDefined &&
          scope.getPrefix(Helper.XSI_URL) != null) scala.xml.Attribute(scope.getPrefix(Helper.XSI_URL), "type", typeName.get,
        writesAttribute(obj, scope))
      else writesAttribute(obj, scope),
      scope,
      writesChildNodes(obj, scope): _*)
}

trait AttributeGroupFormat[A] extends scalaxb.XMLFormat[A] {
  def writes(__obj: A, __namespace: Option[String], __elementLabel: Option[String],
    __scope: scala.xml.NamespaceBinding, __typeAttribute: Boolean): scala.xml.NodeSeq = error("don't call me.")

  def toAttribute(__obj: A, __attr: scala.xml.MetaData, __scope: scala.xml.NamespaceBinding): scala.xml.MetaData
}

trait ElemNameParser[A] extends AnyElemNameParser with XMLFormat[A] with CanWriteChildNodes[A] {
  def reads(seq: scala.xml.NodeSeq, stack: List[ElemName]): Either[String, A] = seq match {
    case node: scala.xml.Node =>
      parse(parser(node, stack), node.child) match {
        case x: Success[_] => Right(x.get)
        case x: Failure => Left(parserErrorMsg(x.msg, x.next, ElemName(node) :: stack))
        case x: Error => Left(parserErrorMsg(x.msg, node))
      }
    case _ => Left("seq must be scala.xml.Node")
  }

  private def parserErrorMsg(msg: String, next: scala.util.parsing.input.Reader[Elem], stack: List[ElemName]): String =
    if (msg contains "paser error ") msg
    else "parser error \"" + msg + "\" while parsing " + stack.reverse.mkString("/", "/", "/") + next.pos.longString

  private def parserErrorMsg(msg: String, node: scala.xml.Node): String =
    if (msg contains "paser error ") msg
    else "parser error \"" + msg + "\" while parsing " + node.toString

  def parser(node: scala.xml.Node, stack: List[ElemName]): Parser[A]
  def isMixed: Boolean = false

  def parse[A](p: Parser[A], in: Seq[scala.xml.Node]): ParseResult[A] =
    p(new ElemNameSeqReader(elementNames(in)))

  def elementNames(in: Seq[scala.xml.Node]): Seq[ElemName] =
    if (isMixed) in map { x => ElemName(x) }
    else in collect { case x: scala.xml.Elem => ElemName(x) }
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
    source.mkString

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

  def instanceType(node: scala.xml.Node): (Option[String], Option[String]) = {
    val typeName = (node \ ("@{" + XSI_URL + "}type")).text
    val prefix = if (typeName.contains(':')) Some(typeName.dropRight(typeName.length - typeName.indexOf(':')))
      else None
    val namespace = Option[String](node.scope.getURI(prefix.orNull))
    val value = if (typeName.contains(':')) typeName.drop(typeName.indexOf(':') + 1)
      else typeName
    (namespace, if (value == "") None else Some(value))
  }

  def getPrefix(namespace: Option[String], scope: scala.xml.NamespaceBinding) =
    if (Option[String](scope.getURI(null)) == namespace) None
    else Option[String](scope.getPrefix(namespace.orNull))

  def stringToXML(obj: String, namespace: Option[String], elementLabel: Option[String],
       scope: scala.xml.NamespaceBinding): scala.xml.NodeSeq = {
    elementLabel map { label =>
      scala.xml.Elem(getPrefix(namespace, scope).orNull, label,
        scala.xml.Null,
        scope, scala.xml.Text(obj.toString))
    } getOrElse { scala.xml.Text(obj) }
  }
}

class ParserFailure(message: String) extends RuntimeException(message)