package org.scalaxb.rt

abstract class DataModel {
  def toXML(elementLabel: String, scope: scala.xml.NamespaceBinding): scala.xml.Node 
}

case class DataRecord[A](key: String, value: A) {
  def toXML(elementLabel: String,
      scope: scala.xml.NamespaceBinding): scala.xml.Node = value match {
    case x: DataModel => x.toXML(elementLabel, scope)
    case _ => scala.xml.Elem(null, elementLabel, scala.xml.Null, scope,
      scala.xml.Text(value.toString))
  }
}

class Calendar extends java.util.GregorianCalendar {
  override def toString: String = Helper.toString(this)
}

object Calendar {
  def apply(value: String): Calendar = Helper.toCalendar(value)
  def unapply(value: Calendar): Option[String] = Some(Helper.toString(value))
}

object Helper {
  lazy val typeFactory = javax.xml.datatype.DatatypeFactory.newInstance()

  def toCalendar(value: String) = {
    val gregorian = typeFactory.newXMLGregorianCalendar(value).toGregorianCalendar
    val cal = new Calendar()

    for (i <- 0 to java.util.Calendar.FIELD_COUNT - 1)
      if (gregorian.isSet(i))
        cal.set(i, gregorian.get(i))
    cal
  }
  
  def toString(value: Calendar) = {
    val xmlGregorian = typeFactory.newXMLGregorianCalendar(value)
    xmlGregorian.toString
  }

  def toDuration(value: String) =
    typeFactory.newDuration(value)
  
  def toByteArray(value: String) =
    (new sun.misc.BASE64Decoder()).decodeBuffer(value)
    
  def toURI(value: String) =
    java.net.URI.create(value)
}
