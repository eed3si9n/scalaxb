package scalaxb.compiler.xsd2

import treehugger.forest._
import definitions._

trait Symbols {
  lazy val XMLPackage          = ScalaPackageClass.newPackage("xml")
  lazy val XMLPackageClass     = ScalaxbPackage.moduleClass
  lazy val TextClass           = XMLPackageClass.newClass("Text")
  lazy val NodeClass           = XMLPackageClass.newClass("Node")
  lazy val NamespaceBindingClass = XMLPackageClass.newClass("NamespaceBinding")

  lazy val ScalaxbPackage      = RootClass.newPackage("scalaxb")
  lazy val ScalaxbPackageClass = ScalaxbPackage.moduleClass
  lazy val Scalaxb_fromXML     = ScalaxbPackageClass.newMethod("fromXML")
  lazy val Scalaxb_toXML       = ScalaxbPackageClass.newMethod("toXML")

  lazy val XMLFormatClass      = ScalaxbPackageClass.newClass("XMLFormat")
  lazy val ElemNameClass       = ScalaxbPackageClass.newClass("ElemName")
  lazy val DataRecordClass     = ScalaxbPackageClass.newClass("DataRecord")

  lazy val ParserClass         = RootClass.newClass("Parser")

  def xmlFormatType(arg: Type): Type = appliedType(XMLFormatClass.typeConstructor, List(arg))
}
