package sbtscalaxb

import sbt._
import scalaxb.{compiler => sc}
import scalaxb.compiler.{Config => ScConfig}
import scalaxb.BuildInfo
import scala.collection.immutable
import sc.ConfigEntry._

object Plugin extends sbt.Plugin {
  import Keys._
  import ScalaxbKeys._

  object ScalaxbKeys {
    lazy val scalaxb          = TaskKey[Seq[File]]("scalaxb")
    lazy val generate         = TaskKey[Seq[File]]("scalaxb-generate")
    lazy val scalaxbConfig    = SettingKey[ScConfig]("scalaxb-config")
    lazy val xsdSource        = SettingKey[File]("scalaxb-xsd-source")
    lazy val wsdlSource       = SettingKey[File]("scalaxb-wsdl-source")
    lazy val packageName      = SettingKey[String]("scalaxb-package-name")
    lazy val packageNames     = SettingKey[Map[URI, String]]("scalaxb-package-names")
    lazy val classPrefix      = SettingKey[Option[String]]("scalaxb-class-prefix")
    lazy val paramPrefix      = SettingKey[Option[String]]("scalaxb-param-prefix")
    lazy val attributePrefix  = SettingKey[Option[String]]("scalaxb-attribute-prefix")
    lazy val prependFamily    = SettingKey[Boolean]("scalaxb-prepend-family")
    lazy val wrapContents     = SettingKey[Seq[String]]("scalaxb-wrap-contents")
    lazy val contentsSizeLimit = SettingKey[Int]("scalaxb-contents-size-limit")
    lazy val chunkSize        = SettingKey[Int]("scalaxb-chunk-size")
    lazy val namedAttributes  = SettingKey[Boolean]("scalaxb-named-attributes")
    lazy val packageDir       = SettingKey[Boolean]("scalaxb-package-dir")
    lazy val generateRuntime  = SettingKey[Boolean]("scalaxb-generate-runtime")
    lazy val protocolFileName = SettingKey[String]("scalaxb-protocol-file-name")
    lazy val protocolPackageName  = SettingKey[Option[String]]("scalaxb-protocol-package-name")
    lazy val laxAny           = SettingKey[Boolean]("scalaxb-lax-any")
    lazy val combinedPackageNames = SettingKey[Map[Option[String], Option[String]]]("scalaxb-combined-package-names")
    lazy val generateDispatchClient = SettingKey[Boolean]("scalaxb-generate-dispatch-client")
    lazy val generateDispatchAs = SettingKey[Boolean]("scalaxb-generate-dispatch-as")
    lazy val dispatchVersion  = SettingKey[String]("scalaxb-dispatch-version")
    lazy val async            = SettingKey[Boolean]("scalaxb-async")
    lazy val ignoreUnknown    = SettingKey[Boolean]("scalaxb-ignore-unknown")
  }

  object ScalaxbCompile {
    def apply(sources: Seq[File], packageName: String, outDir: File, cacheDir: File): Seq[File] =
      apply(sources, sc.Config.default.update(PackageNames(Map(None -> Some(packageName)))), outDir, cacheDir, false)

    def apply(sources: Seq[File], config: sc.Config, outDir: File, cacheDir: File, verbose: Boolean = false): Seq[File] = {
      import sbinary.{DefaultProtocol,Format}
      import DefaultProtocol.{FileFormat, immutableMapFormat, StringFormat, UnitFormat}
      import Tracked.{inputChanged, outputChanged}
      import Types.:+:
      import Cache._
      import FilesInfo.{lastModified, exists}

      def compile: Seq[File] =
        sources.headOption map { src =>
          import sc._
          sc.Log.configureLogger(verbose)
          val module = Module.moduleByFileName(src)
          module.processFiles(sources.toVector, config.update(Outdir(outDir)))
        } getOrElse {Nil}
      def cachedCompile =
        inputChanged(cacheDir / "scalaxb-inputs") { (inChanged, inputs: Seq[File] :+: FilesInfo[ModifiedFileInfo] :+: String :+: HNil) =>
          outputChanged(cacheDir / "scalaxb-output") { (outChanged, outputs: FilesInfo[PlainFileInfo]) =>
            if (inChanged || outChanged) compile
            else outputs.files.toSeq map {_.file}
          }
        }
      def inputs = sources :+: lastModified(sources.toSet) :+: BuildInfo.version :+: HNil
      cachedCompile(inputs)(() => exists((outDir ** "*.scala").get.toSet))
    }
  }

  lazy val scalaxbSettings: Seq[Def.Setting[_]] = inConfig(Compile)(baseScalaxbSettings)
  lazy val baseScalaxbSettings: Seq[Def.Setting[_]] = Seq(
    scalaxb := (generate in scalaxb).value,
    sourceManaged in scalaxb := {
      sourceManaged.value / "sbt-scalaxb"
    },
    xsdSource in scalaxb := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "xsd"
      else src / "main" / "xsd"
    },
    wsdlSource in scalaxb := {
      val src = sourceDirectory.value
      if (Seq(Compile, Test) contains configuration.value) src / "wsdl"
      else src / "main" / "wsdl"
    },
    logLevel in scalaxb <<= logLevel?? Level.Info
  ) ++ inTask(scalaxb)(Seq(
    generate := {
      val s = streams.value
      val ll = logLevel.value
      ScalaxbCompile(sources.value, scalaxbConfig.value, sourceManaged.value, s.cacheDirectory, ll == Level.Debug)
    },
    sources := {
      val xsd = xsdSource.value
      val wsdl = wsdlSource.value
      (wsdl ** "*.wsdl").get.sorted ++ (xsd ** "*.xsd").get.sorted
    },
    clean := {
      val outdir = sourceManaged.value
      IO.delete((outdir ** "*").get)
      IO.createDirectory(outdir)
    },
    combinedPackageNames := {
      val x = packageName.value
      val xs = packageNames.value
      (xs map { case (k, v) => ((Some(k.toString): Option[String]), Some(v)) }) updated (None, Some(x))
    },
    packageName             := "generated",
    packageNames            := Map(),
    classPrefix             := None,
    paramPrefix             := None,
    attributePrefix         := None,
    prependFamily           := false,
    wrapContents            := Nil,
    contentsSizeLimit       := Int.MaxValue,
    chunkSize               := 10,
    namedAttributes         := false,
    packageDir              := true,
    generateRuntime         := true,
    generateDispatchClient  := true,
    generateDispatchAs      := false,
    protocolFileName        := sc.Defaults.protocolFileName,
    protocolPackageName     := None,
    laxAny                  := false,
    dispatchVersion         := ScConfig.defaultDispatchVersion.value,
    async in scalaxb        := true,
    ignoreUnknown           := false,
    scalaxbConfig :=
      ScConfig(
        Vector(PackageNames(combinedPackageNames.value)) ++
        (if (packageDir.value) Vector(GeneratePackageDir) else Vector()) ++
        (classPrefix.value match {
          case Some(x) => Vector(ClassPrefix(x))
          case None    => Vector()
        }) ++
        (paramPrefix.value match {
          case Some(x) => Vector(ParamPrefix(x))
          case None    => Vector()
        }) ++
        (attributePrefix.value match {
          case Some(x) => Vector(AttributePrefix(x))
          case None    => Vector()
        }) ++
        Vector(ScConfig.defaultOutdir) ++
        (if (prependFamily.value) Vector(PrependFamilyName) else Vector()) ++
        Vector(WrappedComplexTypes(wrapContents.value.toList)) ++
        Vector(SeperateProtocol) ++
        Vector(ProtocolFileName(protocolFileName.value)) ++
        Vector(ProtocolPackageName(protocolPackageName.value)) ++
        Vector(ScConfig.defaultDefaultNamespace) ++
        (if (generateRuntime.value) Vector(GenerateRuntime) else Vector()) ++
        (if (generateDispatchClient.value) Vector(GenerateDispatchClient) else Vector()) ++
        (if (generateDispatchAs.value) Vector(GenerateDispatchAs) else Vector()) ++
        Vector(ContentsSizeLimit(contentsSizeLimit.value)) ++
        Vector(SequenceChunkSize(chunkSize.value)) ++
        (if (namedAttributes.value) Vector(NamedAttributes) else Vector()) ++
        (if (laxAny.value) Vector(LaxAny) else Vector()) ++
        Vector(DispatchVersion(dispatchVersion.value)) ++
        (if (async.value) Vector(GenerateAsync) else Vector()) ++
        (if (ignoreUnknown.value) Vector(IgnoreUnknown) else Vector())
      )
  ))
}
