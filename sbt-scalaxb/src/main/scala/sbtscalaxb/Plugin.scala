package sbtscalaxb

import sbt._
import scalaxb.{compiler => sc}
import scalaxb.compiler.{Config => ScConfig}

object Plugin extends sbt.Plugin {
  import Keys._
  import ScalaxbKeys._

  object ScalaxbKeys {
    lazy val scalaxb          = TaskKey[Seq[File]]("scalaxb")
    lazy val generate         = TaskKey[Seq[File]]("scalaxb-generate")
    lazy val scalaxbConfig    = SettingKey[ScConfig]("scalaxb-config")
    lazy val scalaxbConfig1   = SettingKey[Config1]("scalaxb-config1")
    lazy val scalaxbConfig2   = SettingKey[Config2]("scalaxb-config2")
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
    lazy val packageDir       = SettingKey[Boolean]("scalaxb-package-dir")
    lazy val generateRuntime  = SettingKey[Boolean]("scalaxb-generate-runtime")
    lazy val protocolFileName = SettingKey[String]("scalaxb-protocol-file-name")
    lazy val protocolPackageName  = SettingKey[Option[String]]("scalaxb-protocol-package-name")
    lazy val laxAny           = SettingKey[Boolean]("scalaxb-lax-any")
    lazy val combinedPackageNames = SettingKey[Map[Option[String], Option[String]]]("scalaxb-combined-package-names")
    lazy val dispatchVersion  = SettingKey[String]("scalaxb-dispatch-version")
  }

  object ScalaxbCompile {
    def apply(sources: Seq[File], packageName: String, outDir: File, cacheDir: File): Seq[File] =
      apply(sources, sc.Config(packageNames = Map(None -> Some(packageName))), outDir, cacheDir, false)

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
          module.processFiles(sources, config.copy(outdir = outDir))
        } getOrElse {Nil}
      def cachedCompile =
        inputChanged(cacheDir / "scalaxb-inputs") { (inChanged, inputs: Seq[File] :+: FilesInfo[ModifiedFileInfo] :+: HNil) =>
          outputChanged(cacheDir / "scalaxb-output") { (outChanged, outputs: FilesInfo[PlainFileInfo]) =>
            if (inChanged || outChanged) compile
            else outputs.files.toSeq map {_.file}
          }
        }
      val inputs = sources :+: lastModified(sources.toSet) :+: HNil
      cachedCompile(inputs)(() => exists((outDir ** "*.scala").get.toSet))
    }
  }

  lazy val scalaxbSettings: Seq[Project.Setting[_]] = inConfig(Compile)(baseScalaxbSettings)
  lazy val baseScalaxbSettings: Seq[Project.Setting[_]] = Seq(
    scalaxb <<= generate in scalaxb,
    generate in scalaxb <<= (sources in scalaxb, scalaxbConfig in scalaxb,
        sourceManaged in scalaxb, cacheDirectory, logLevel in scalaxb) map { (sources, config, outdir, cacheDir, ll) =>
      ScalaxbCompile(sources, config, outdir, cacheDir, ll == Level.Debug) },
    sourceManaged in scalaxb <<= sourceManaged / "sbt-scalaxb",
    sources in scalaxb <<= (xsdSource in scalaxb, wsdlSource in scalaxb) map { (xsd, wsdl) =>
      (wsdl ** "*.wsdl").get.sorted ++ (xsd ** "*.xsd").get.sorted },
    xsdSource in scalaxb <<= (sourceDirectory, configuration) { (src, config) =>
      if (Seq(Compile, Test) contains config) src / "xsd"
      else src / "main" / "xsd" },
    wsdlSource in scalaxb <<= (sourceDirectory, configuration) { (src, config) =>
      if (Seq(Compile, Test) contains config) src / "wsdl"
      else src / "main" / "wsdl" },
    clean in scalaxb <<= (sourceManaged in scalaxb) map { (outdir) =>
      IO.delete((outdir ** "*").get)
      IO.createDirectory(outdir) },
    packageName in scalaxb := "generated",
    packageNames in scalaxb := Map(),
    classPrefix in scalaxb := None,
    paramPrefix in scalaxb := None,
    attributePrefix in scalaxb := None,
    prependFamily in scalaxb := false,
    wrapContents in scalaxb := Nil,
    contentsSizeLimit in scalaxb := 20,
    chunkSize in scalaxb := 10,
    packageDir in scalaxb := true,
    generateRuntime in scalaxb := true,
    protocolFileName in scalaxb := sc.Defaults.protocolFileName,
    protocolPackageName in scalaxb := None,
    laxAny in scalaxb := false,
    dispatchVersion in scalaxb := "0.10.1",
    combinedPackageNames in scalaxb <<= (packageName in scalaxb, packageNames in scalaxb) { (x, xs) =>
      (xs map { case (k, v) => ((Some(k.toString): Option[String]), Some(v)) }) updated (None, Some(x)) },


    scalaxbConfig1 in scalaxb <<= (combinedPackageNames in scalaxb,
        packageDir in scalaxb,
        classPrefix in scalaxb,
        paramPrefix in scalaxb,
        attributePrefix in scalaxb,
        prependFamily in scalaxb,
        wrapContents in scalaxb) { (pkg, pkgdir, cpre, ppre, apre, pf, w) =>
      Config1(packageNames = pkg,
        packageDir = pkgdir,
        classPrefix = cpre,
        classPostfix = None,
        paramPrefix = ppre,
        attributePrefix = apre,
        outdir = new File("."),
        prependFamilyName = pf, 
        wrappedComplexTypes = w.toList) },
    scalaxbConfig2 in scalaxb <<= (contentsSizeLimit in scalaxb,
        generateRuntime in scalaxb,
        chunkSize in scalaxb,
        protocolFileName in scalaxb,
        protocolPackageName in scalaxb,
        laxAny in scalaxb,
        dispatchVersion in scalaxb) { (csl, rt, cs, pfn, ppn, la, dv) =>
      Config2(seperateProtocol = true,
        protocolFileName = pfn,
        protocolPackageName = ppn,
        defaultNamespace = None,
        generateRuntime = rt,
        contentsSizeLimit = csl,
        sequenceChunkSize = cs,
        laxAny = la,
        dispatchVersion = dv) },

    scalaxbConfig in scalaxb <<= (scalaxbConfig1 in scalaxb, scalaxbConfig2 in scalaxb) { (c1, c2) =>
      ScConfig(packageNames = c1.packageNames,
        packageDir = c1.packageDir,
        classPrefix = c1.classPrefix,
        paramPrefix = c1.paramPrefix,
        attributePrefix = c1.attributePrefix,
        prependFamilyName = c1.prependFamilyName,
        wrappedComplexTypes = c1.wrappedComplexTypes,
        contentsSizeLimit = c2.contentsSizeLimit,
        generateRuntime = c2.generateRuntime,
        sequenceChunkSize = c2.sequenceChunkSize,
        protocolFileName = c2.protocolFileName,
        protocolPackageName = c2.protocolPackageName,
        laxAny = c2.laxAny,
        dispatchVersion = c2.dispatchVersion) },

    logLevel in scalaxb <<= logLevel?? Level.Info
  )
}

case class Config1(packageNames: Map[Option[String], Option[String]],
  classPrefix: Option[String],
  classPostfix: Option[String],
  paramPrefix: Option[String],
  attributePrefix: Option[String],
  outdir: File,
  packageDir: Boolean,
  wrappedComplexTypes: List[String],
  prependFamilyName: Boolean) {}

case class Config2(seperateProtocol: Boolean,
  protocolFileName: String,
  protocolPackageName: Option[String],
  defaultNamespace: Option[String],
  generateRuntime: Boolean,
  contentsSizeLimit: Int,
  sequenceChunkSize: Int,
  laxAny: Boolean,
  dispatchVersion: String) {}
