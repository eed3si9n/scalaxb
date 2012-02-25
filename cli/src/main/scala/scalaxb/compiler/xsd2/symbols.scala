package scalaxb.compiler.xsd2

import treehugger.forest._
import definitions._
import treehuggerDSL._

trait Symbols {
  lazy val NamespaceBindingClass = XmlPackageClass.newClass("NamespaceBinding")

  lazy val ScalaxbPackage      = RootClass.newPackage("scalaxb")
  lazy val ScalaxbPackageClass = ScalaxbPackage.moduleClass
  lazy val Scalaxb_fromXML     = ScalaxbPackageClass.newMethod("fromXML")
  lazy val Scalaxb_toXML       = ScalaxbPackageClass.newMethod("toXML")

  lazy val Base64BinaryClass   = ScalaxbPackageClass.newClass("Base64Binary")
  lazy val HexBinaryClass      = ScalaxbPackageClass.newClass("HexBinary")
  lazy val XMLFormatClass      = ScalaxbPackageClass.newClass("XMLFormat")
  lazy val ElemNameClass       = ScalaxbPackageClass.newClass("ElemName")
  lazy val ElemNameParserClass = ScalaxbPackageClass.newClass("ElemNameParser")
  lazy val DataRecordClass     = ScalaxbPackageClass.newClass("DataRecord")
  lazy val DataRecordAnyClass  = DataRecordClass TYPE_OF AnyClass
  lazy val DataRecordOptionAnyClass = DataRecordClass TYPE_OF TYPE_OPTION(AnyClass)
  lazy val MapStringDataRecordAnyClass =
    TYPE_MAP(StringClass, DataRecordAnyClass)

  lazy val ParserClass         = RootClass.newClass("Parser")

  lazy val JavaxXmlDataTypePackage  = JavaxXmlPackageClass.newPackage("datatype")
  lazy val JavaxXmlDataTypePackageClass = JavaxXmlDataTypePackage.moduleClass
  lazy val DurationClass       = JavaxXmlDataTypePackageClass.newClass("Duration")
  lazy val XMLGregorianCalendarClass = JavaxXmlDataTypePackageClass.newClass("XMLGregorianCalendar")
  lazy val JavaxXmlNamespacePackage  = JavaxXmlPackageClass.newPackage("namespace")
  lazy val JavaxXmlNamespacePackageClass = JavaxXmlNamespacePackage.moduleClass
  lazy val QNameClass          = JavaxXmlNamespacePackageClass.newClass("QName")

  def xmlFormatType(arg: Type): Type = appliedType(XMLFormatClass.typeConstructor, List(arg))
}
