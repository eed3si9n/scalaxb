package sbtscalaxb

import sbt._
import Keys._
import scala.collection.immutable
import scalaxb.{compiler => sc}
import scalaxb.compiler.{Config => ScConfig}
import sc.ConfigEntry._

object ScalaxbPlugin extends sbt.AutoPlugin {
  override def requires = plugins.JvmPlugin

  object autoImport extends ScalaxbKeys
  import autoImport._

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(baseScalaxbSettings) ++
    Set(
      sourceGenerators in Compile += (scalaxb in Compile).taskValue
    )
  lazy val baseScalaxbSettings: Seq[Def.Setting[_]] = Seq(
    scalaxb := (scalaxbGenerate in scalaxb).value,
    sourceManaged in scalaxb := {
      sourceManaged.value / "sbt-scalaxb"
    },
    scalaxbXsdSource in scalaxb := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "xsd"
      else src / "main" / "xsd"
    },
    scalaxbWsdlSource in scalaxb := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "wsdl"
      else src / "main" / "wsdl"
    },
    logLevel in scalaxb := (logLevel?? Level.Info).value
  ) ++ inTask(scalaxb)(Seq(
    scalaxbGenerate := {
      val s = streams.value
      val ll = logLevel.value
      ScalaxbCompile(sources.value, scalaxbConfig.value, sourceManaged.value, s.cacheDirectory, ll == Level.Debug)
    },
    sources := {
      val xsd = scalaxbXsdSource.value
      val wsdl = scalaxbWsdlSource.value
      (wsdl ** "*.wsdl").get.sorted ++ (xsd ** "*.xsd").get.sorted
    },
    clean := {
      val outdir = sourceManaged.value
      IO.delete((outdir ** "*").get)
      IO.createDirectory(outdir)
    },
    scalaxbCombinedPackageNames := {
      val x = scalaxbPackageName.value
      val xs = scalaxbPackageNames.value
      (xs map { case (k, v) => ((Some(k.toString): Option[String]), Some(v)) }) updated (None, Some(x))
    },
    scalaxbPackageName             := "generated",
    scalaxbPackageNames            := Map(),
    scalaxbClassPrefix             := None,
    scalaxbParamPrefix             := None,
    scalaxbAttributePrefix         := None,
    scalaxbOpOutputWrapperPostfix  := sc.Defaults.opOutputWrapperPostfix,
    scalaxbPrependFamily           := false,
    scalaxbWrapContents            := Nil,
    scalaxbContentsSizeLimit       := Int.MaxValue,
    scalaxbChunkSize               := 10,
    scalaxbNamedAttributes         := false,
    scalaxbPackageDir              := true,
    scalaxbGenerateRuntime         := true,
    scalaxbGenerateDispatchClient  := true,
    scalaxbGenerateDispatchAs      := false,
    scalaxbGenerateGigahorseClient := false,
    scalaxbGenerateSingleClient    := HttpClientType.None,
    scalaxbProtocolFileName        := sc.Defaults.protocolFileName,
    scalaxbProtocolPackageName     := None,
    scalaxbLaxAny                  := false,
    scalaxbDispatchVersion         := ScConfig.defaultDispatchVersion.value,
    scalaxbGigahorseVersion        := ScConfig.defaultGigahorseVersion.value,
    scalaxbGigahorseBackend        := GigahorseHttpBackend.OkHttp,
    scalaxbAsync                   := true,
    scalaxbIgnoreUnknown           := false,
    scalaxbVararg                  := false,
    scalaxbGenerateMutable         := false,
    scalaxbGenerateVisitor         := false,
    scalaxbAutoPackages            := false,
    scalaxbCapitalizeWords         := false,
    scalaxbSymbolEncodingStrategy  := SymbolEncodingStrategy.Legacy151,
    scalaxbEnumNameMaxLength       := 50,
    scalaxbConfig :=
      ScConfig(
        Vector(PackageNames(scalaxbCombinedPackageNames.value)) ++
        (if (scalaxbPackageDir.value) Vector(GeneratePackageDir) else Vector()) ++
        (scalaxbClassPrefix.value match {
          case Some(x) => Vector(ClassPrefix(x))
          case None    => Vector()
        }) ++
        (scalaxbParamPrefix.value match {
          case Some(x) => Vector(ParamPrefix(x))
          case None    => Vector()
        }) ++
        (scalaxbAttributePrefix.value match {
          case Some(x) => Vector(AttributePrefix(x))
          case None    => Vector()
        }) ++
        Vector(OpOutputWrapperPostfix(scalaxbOpOutputWrapperPostfix.value)) ++
        Vector(ScConfig.defaultOutdir) ++
        (if (scalaxbPrependFamily.value) Vector(PrependFamilyName) else Vector()) ++
        Vector(WrappedComplexTypes(scalaxbWrapContents.value.toList)) ++
        Vector(SeperateProtocol) ++
        Vector(ProtocolFileName(scalaxbProtocolFileName.value)) ++
        Vector(ProtocolPackageName(scalaxbProtocolPackageName.value)) ++
        Vector(ScConfig.defaultDefaultNamespace) ++
        (if (scalaxbGenerateRuntime.value) Vector(GenerateRuntime) else Vector()) ++
        (if (scalaxbGenerateDispatchClient.value && scalaxbGenerateSingleClient.value == HttpClientType.None ||
          scalaxbGenerateSingleClient.value == HttpClientType.Dispatch) Vector(GenerateDispatchClient) else Vector()) ++
        (if (scalaxbGenerateDispatchAs.value) Vector(GenerateDispatchAs) else Vector()) ++
        (if (scalaxbGenerateGigahorseClient.value && scalaxbGenerateSingleClient.value == HttpClientType.None ||
          scalaxbGenerateSingleClient.value == HttpClientType.Gigahorse) Vector(GenerateGigahorseClient) else Vector()) ++
        Vector(ContentsSizeLimit(scalaxbContentsSizeLimit.value)) ++
        Vector(SequenceChunkSize(scalaxbChunkSize.value)) ++
        (if (scalaxbNamedAttributes.value) Vector(NamedAttributes) else Vector()) ++
        (if (scalaxbLaxAny.value) Vector(LaxAny) else Vector()) ++
        Vector(DispatchVersion(scalaxbDispatchVersion.value)) ++
        Vector(GigahorseVersion(scalaxbGigahorseVersion.value)) ++
        Vector(GigahorseBackend(scalaxbGigahorseBackend.value.toString)) ++
        (if (scalaxbAsync.value) Vector(GenerateAsync) else Vector()) ++
        (if (scalaxbIgnoreUnknown.value) Vector(IgnoreUnknown) else Vector()) ++
        (if (scalaxbVararg.value && !scalaxbGenerateMutable.value) Vector(VarArg) else Vector()) ++
        (if (scalaxbGenerateMutable.value) Vector(GenerateMutable) else Vector()) ++
        (if (scalaxbGenerateVisitor.value) Vector(GenerateVisitor) else Vector()) ++
        (if (scalaxbAutoPackages.value) Vector(AutoPackages) else Vector()) ++
        (if (scalaxbCapitalizeWords.value) Vector(CapitalizeWords) else Vector()) ++
        Vector(SymbolEncoding.withName(scalaxbSymbolEncodingStrategy.value.toString)) ++
        Vector(EnumNameMaxLength(scalaxbEnumNameMaxLength.value))
      )
  ))
}
