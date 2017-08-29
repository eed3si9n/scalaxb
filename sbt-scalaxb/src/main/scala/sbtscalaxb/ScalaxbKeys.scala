package sbtscalaxb

import sbt._
import Keys._

import scalaxb.compiler.{Config => ScConfig}

trait ScalaxbKeys {
  lazy val scalaxb                 = taskKey[Seq[File]]("Generates case classes and typeclass instances")
  lazy val scalaxbGenerate         = taskKey[Seq[File]]("Generates case classes and typeclass instances")
  lazy val scalaxbConfig           = settingKey[ScConfig]("Configuration for scalaxb")
  lazy val scalaxbXsdSource        = settingKey[File]("xsd source directory")
  lazy val scalaxbWsdlSource       = settingKey[File]("wsdl source directory")
  lazy val scalaxbPackageName      = settingKey[String]("Specifies the target package")
  lazy val scalaxbPackageNames     = settingKey[Map[URI, String]]("specifies the target package")
  lazy val scalaxbAutoPackages     = settingKey[Boolean]("Generates packages for different namespaces automatically")
  lazy val scalaxbClassPrefix      = settingKey[Option[String]]("Prefixes generated class names")
  lazy val scalaxbUseCamelCase     = settingKey[Boolean]("Translates class and member names to camel case")
  lazy val scalaxbParamPrefix      = settingKey[Option[String]]("Prefixes generated parameter names")
  lazy val scalaxbAttributePrefix  = settingKey[Option[String]]("Prefixes generated attribute parameters")
  lazy val scalaxbOpOutputWrapperPostfix = settingKey[String]("Postfixes operation output wrapper names (default: Output)")
  lazy val scalaxbPrependFamily    = settingKey[Boolean]("Prepends family name to class names")
  lazy val scalaxbWrapContents     = settingKey[Seq[String]]("Wraps inner contents into a separate case class")
  lazy val scalaxbContentsSizeLimit = settingKey[Int]("Defines long contents to be segmented (default: max)")
  lazy val scalaxbChunkSize        = settingKey[Int]("Segments long sequences into chunks (default: 10)")
  lazy val scalaxbNamedAttributes  = settingKey[Boolean]("Generates named fields for attributes")
  lazy val scalaxbPackageDir       = settingKey[Boolean]("Generates package directories")
  lazy val scalaxbGenerateRuntime  = settingKey[Boolean]("Generates scalaxb runtime (scalaxb.scala)")
  lazy val scalaxbProtocolFileName = settingKey[String]("Protocol file name (xmlprotocol.scala)")
  lazy val scalaxbProtocolPackageName  = settingKey[Option[String]]("Package for protocols")
  lazy val scalaxbGenerateMutable  = settingKey[Boolean]("Generates mutable classes")
  lazy val scalaxbGenerateVisitor  = settingKey[Boolean]("Generates visitor")
  lazy val scalaxbLaxAny           = settingKey[Boolean]("Relaxes namespace constraints of xs:any")
  lazy val scalaxbCombinedPackageNames = settingKey[Map[Option[String], Option[String]]]("")
  lazy val scalaxbGenerateDispatchClient = settingKey[Boolean]("Generate of Dispatch client")
  lazy val scalaxbGenerateDispatchAs = settingKey[Boolean]("Generates Dispatch \"as\"")
  lazy val scalaxbGenerateGigahorseClient = settingKey[Boolean]("Generate of Gigahorse client")
  lazy val scalaxbGenerateSingleClient = settingKey[HttpClientType.Value]("Generate a single client (one of Dispatch or Gigahorse)")
  lazy val scalaxbDispatchVersion  = settingKey[String]("Dispatch version")
  lazy val scalaxbGigahorseVersion = settingKey[String]("Gigahorse version")
  lazy val scalaxbGigahorseBackend = settingKey[GigahorseHttpBackend.Value]("Gigahorse http backend")
  lazy val scalaxbAsync            = settingKey[Boolean]("Generates async SOAP client")
  lazy val scalaxbIgnoreUnknown    = settingKey[Boolean]("Ignores unknown Elements")
  lazy val scalaxbVararg           = settingKey[Boolean]("Uses varargs when possible. (default: false)")

  object HttpClientType extends Enumeration {
    val None, Dispatch, Gigahorse = Value
  }

  object GigahorseHttpBackend extends Enumeration {
    val OkHttp = Value("okhttp")
    val AHC = Value("asynchttpclient")
    //val AkkaHttp = Value("akkahttp")
  }
}
object ScalaxbKeys extends ScalaxbKeys
