package org.scalaxb.rt

case class DataRecord[+A](namespace: String, key: String, value: A)
    (implicit m : scala.reflect.Manifest[A]) {
  type DataModel = {
    def toXML(namespace: String, elementLabel: String,
      scope: scala.xml.NamespaceBinding): scala.xml.Node 
  }
  
  def toXML(namespace: String, elementLabel: String,
      scope: scala.xml.NamespaceBinding): scala.xml.Node =
    try
      value.asInstanceOf[DataModel].toXML(namespace, elementLabel, scope)
    catch {
      case _ => 
        value match {
          case x: scala.xml.Node => x
          case x: String if key == null => scala.xml.Text(x)

          // for now the structural type does not work within pattern matching,
          // but it might in the future.
          // case x: DataModel => x.toXML(namespace, elementLabel, scope)
          case x => scala.xml.Elem(scope.getPrefix(namespace), elementLabel,
                scala.xml.Null, scope, scala.xml.Text(value.toString))
        }
    } // catch
}

case class ElemName(namespace: String, name: String) {
  var node: scala.xml.Node = _
  def text = node.text
  def nil = Helper.isNil(node)
  def toDataRecord = DataRecord(namespace, name, node)
}

trait ElemNameParser[A] extends scala.util.parsing.combinator.Parsers {
  type Elem = ElemName

  def fromXML(node: scala.xml.Node): A =
    parse(parser(node), node.child.collect { case elem: scala.xml.Elem => elem }) match {
      case x: Success[_] => x.get
      case x: Failure => error("fromXML failed: " + x.msg)
      case x: Error => error("fromXML errored: " + x.msg)
    }
  
  def parser(node: scala.xml.Node): Parser[A]
  
  def parse[A](p: Parser[A], in: Seq[scala.xml.Elem]): ParseResult[A] = 
    parseElemNames(p, in.map(toElemName(_)) )
  
  def toElemName(x: scala.xml.Elem) = {
    val elemName = ElemName(x.scope.getURI(x.prefix), x.label)
    elemName.node = x
    elemName 
  }
  
  def parseElemNames[A](p: Parser[A], in: Seq[ElemName]): ParseResult[A] = 
    p(new ElemNameSeqReader(in))
    
  def any: Parser[ElemName] = 
    accept("any", { case x: ElemName => x })
}

class ElemNameSeqReader(seq: Seq[ElemName],
    override val offset: Int) extends scala.util.parsing.input.Reader[ElemName] {
  import scala.util.parsing.input._
  
  def this(seq: Seq[ElemName]) = this(seq, 0)
  
  override def first: ElemName  =
    if (seq.isDefinedAt(offset))
      seq(offset)
    else
      null
      //error("ElemNameSeqReader#first: no element at position " + offset + " of " + seq)
  
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
    (node \ "@{http://www.w3.org/2001/XMLSchema-instance}nil").headOption match {
      case None    => false
      case Some(x) => x.text == "true" 
    }

  def nilElem(namespace: String, elementLabel: String,
      scope: scala.xml.NamespaceBinding) =
    scala.xml.Elem(scope.getPrefix(namespace), elementLabel,
      scala.xml.Attribute(scope.getPrefix(XSI_URL), "nil", "true", scala.xml.Null),
      scope, Nil: _*)
}
