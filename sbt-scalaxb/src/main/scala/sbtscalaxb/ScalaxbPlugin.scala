package sbtscalaxb

import sbt._
import Keys._
import scala.collection.immutable
import scalaxb.{compiler => sc}
import scalaxb.compiler.{Config => ScConfig}
import sc.ConfigEntry
import sc.ConfigEntry.{HttpClientStyle => _, _}

object ScalaxbPlugin extends sbt.AutoPlugin {
  override def requires = plugins.JvmPlugin

  object autoImport extends ScalaxbKeys
  import autoImport._
  override lazy val globalSettings: Seq[Def.Setting[_]] = Seq(
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
    scalaxbGenerateHttp4sClient    := false,
    scalaxbGenerateGigahorseClient := false,
    scalaxbGenerateSingleClient    := HttpClientType.None,
    scalaxbProtocolFileName        := sc.Defaults.protocolFileName,
    scalaxbProtocolPackageName     := None,
    scalaxbLaxAny                  := false,
    scalaxbDispatchVersion         := ScConfig.defaultDispatchVersion.value,
    scalaxbGigahorseVersion        := ScConfig.defaultGigahorseVersion.value,
    scalaxbGigahorseBackend        := GigahorseHttpBackend.OkHttp,
    scalaxbHttp4sVersion           := ScConfig.defaultHttp4sVersion.value,
    scalaxbMapK                    := false,
    scalaxbIgnoreUnknown           := false,
    scalaxbVararg                  := false,
    scalaxbGenerateMutable         := false,
    scalaxbGenerateVisitor         := false,
    scalaxbGenerateLens            := false,
    scalaxbAutoPackages            := false,
    scalaxbCapitalizeWords         := false,
    scalaxbSymbolEncodingStrategy  := SymbolEncodingStrategy.Legacy151,
    scalaxbEnumNameMaxLength       := 50,
    scalaxbUseLists                := false,
    scalaxbAsync                   := true,
    scalaxbJaxbPackage             := JaxbPackage.Javax,
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(baseScalaxbSettings) ++
    Set(
      Compile / sourceGenerators += (Compile / scalaxb).taskValue
    )
  lazy val baseScalaxbSettings: Seq[Def.Setting[_]] = Seq(
    scalaxb := (scalaxb / scalaxbGenerate).value,
    scalaxb / sourceManaged := {
      sourceManaged.value / "sbt-scalaxb"
    },
    scalaxb / scalaxbXsdSource := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "xsd"
      else src / "main" / "xsd"
    },
    scalaxb / scalaxbWsdlSource := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "wsdl"
      else src / "main" / "wsdl"
    },
    scalaxb / logLevel := (logLevel?? Level.Info).value
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
    scalaxbHttpClientStyle := {
      (scalaxbHttpClientStyle.?.value) match {
        case Some(x) => x
        case _ =>
          if (scalaxbGenerateHttp4sClient.value) HttpClientStyle.Tagless
          else if (scalaxbAsync.value) HttpClientStyle.Future
          else HttpClientStyle.Sync
      }
    },
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
        (if (scalaxbGenerateHttp4sClient.value && scalaxbGenerateSingleClient.value == HttpClientType.None ||
          scalaxbGenerateSingleClient.value == HttpClientType.Http4s) Vector(GenerateHttp4sClient, ConfigEntry.HttpClientStyle.Tagless) else Vector()) ++
        Vector(ContentsSizeLimit(scalaxbContentsSizeLimit.value)) ++
        Vector(SequenceChunkSize(scalaxbChunkSize.value)) ++
        (if (scalaxbNamedAttributes.value) Vector(NamedAttributes) else Vector()) ++
        (if (scalaxbLaxAny.value) Vector(LaxAny) else Vector()) ++
        Vector(DispatchVersion(scalaxbDispatchVersion.value)) ++
        Vector(Http4sVersion(scalaxbHttp4sVersion.value)) ++
        Vector(GigahorseVersion(scalaxbGigahorseVersion.value)) ++
        Vector(GigahorseBackend(scalaxbGigahorseBackend.value.toString)) ++
        (if (scalaxbIgnoreUnknown.value) Vector(IgnoreUnknown) else Vector()) ++
        (if (scalaxbVararg.value && !scalaxbGenerateMutable.value) Vector(VarArg) else Vector()) ++
        (if (scalaxbGenerateMutable.value) Vector(GenerateMutable) else Vector()) ++
        (if (scalaxbGenerateVisitor.value) Vector(GenerateVisitor) else Vector()) ++
        (if (scalaxbGenerateLens.value) Vector(GenerateLens) else Vector()) ++
        (if (scalaxbAutoPackages.value) Vector(AutoPackages) else Vector()) ++
        (if (scalaxbCapitalizeWords.value) Vector(CapitalizeWords) else Vector()) ++
        Vector(SymbolEncoding.withName(scalaxbSymbolEncodingStrategy.value.toString)) ++
        Vector(EnumNameMaxLength(scalaxbEnumNameMaxLength.value)) ++
        (if (scalaxbMapK.value) Vector(GenerateMapK) else Vector()) ++
        (if (scalaxbUseLists.value) Vector(UseLists) else Vector()) ++
        Vector(ConfigEntry.JaxbPackage.withPackageName(scalaxbJaxbPackage.value.toString)) ++
          Vector(scalaxbHttpClientStyle.value match {
            case HttpClientStyle.Sync => ConfigEntry.HttpClientStyle.Sync
            case HttpClientStyle.Future => ConfigEntry.HttpClientStyle.Future
            case HttpClientStyle.Tagless => ConfigEntry.HttpClientStyle.Tagless
          })
  )))
}
