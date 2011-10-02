package sbtscalaxb

import sbt._
import scalaxb.{compiler => sc}

object Plugin extends sbt.Plugin {
  import Keys._
  import ScalaxbKeys._

  object ScalaxbKeys {
    lazy val scalaxb          = TaskKey[Seq[File]]("scalaxb")
    lazy val generate         = TaskKey[Seq[File]]("scalaxb-generate")
    lazy val scalaxbConfig    = SettingKey[sc.Config]("scalaxb-config")
    lazy val xsdSource        = SettingKey[File]("scalaxb-xsd-source")
    lazy val wsdlSource       = SettingKey[File]("scalaxb-wsdl-source")
    lazy val packageName      = SettingKey[String]("scalaxb-package-name")
    lazy val packageNames     = SettingKey[Map[URI, String]]("scalaxb-package-names")
    lazy val classPrefix      = SettingKey[Option[String]]("scalaxb-class-prefix")
    lazy val paramPrefix      = SettingKey[Option[String]]("scalaxb-param-prefix")
    lazy val wrapContents     = SettingKey[Seq[String]]("scalaxb-wrap-contents")
    lazy val chunkSize        = SettingKey[Int]("scalaxb-chunk-size")
    lazy val packageDir       = SettingKey[Boolean]("scalaxb-package-dir")
    lazy val generateRuntime  = SettingKey[Boolean]("scalaxb-generate-runtime")
    lazy val protocolFileName = SettingKey[String]("scalaxb-protocol-file-name")
    lazy val protocolPackageName  = SettingKey[Option[String]]("scalaxb-protocol-package-name")
    lazy val combinedPackageNames = SettingKey[Map[Option[String], Option[String]]]("scalaxb-combined-package-names")
  }

  object ScalaxbCompile {
    def apply(sources: Seq[File], packageName: String, outdir: File): Seq[File] =
      apply(sources, sc.Config(packageNames = Map(None -> Some(packageName))), outdir, false)

    def apply(sources: Seq[File], config: sc.Config, outdir: File, verbose: Boolean = false): Seq[File] =
      sources.headOption map { src =>
        import sc._
        val module = Module.moduleByFileName(src, verbose)
        module.processFiles(sources, config.copy(outdir = outdir))
      } getOrElse {Nil}
  }

  lazy val scalaxbSettings: Seq[Project.Setting[_]] = inConfig(Compile)(baseScalaxbSettings)
  lazy val baseScalaxbSettings: Seq[Project.Setting[_]] = Seq(
    scalaxb <<= generate in scalaxb,
    generate in scalaxb <<= (sources in scalaxb, scalaxbConfig in scalaxb,
        sourceManaged in scalaxb, logLevel in scalaxb) map { (sources, config, outdir, ll) =>
      ScalaxbCompile(sources, config, outdir, ll == Level.Debug) },
    sourceManaged in scalaxb <<= sourceManaged,
    sources in scalaxb <<= (xsdSource in scalaxb, wsdlSource in scalaxb) map { (xsd, wsdl) =>
      (xsd ** "*.xsd").get ++ (wsdl ** "*.wsdl").get },
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
    wrapContents in scalaxb := Nil,
    chunkSize in scalaxb := 10,
    packageDir in scalaxb := true,
    generateRuntime in scalaxb := true,
    protocolFileName in scalaxb := sc.Defaults.protocolFileName,
    protocolPackageName in scalaxb := None,
    combinedPackageNames in scalaxb <<= (packageName in scalaxb, packageNames in scalaxb) { (x, xs) =>
      (xs map { case (k, v) => ((Some(k.toString): Option[String]), Some(v)) }) updated (None, Some(x)) },
    scalaxbConfig in scalaxb <<= (combinedPackageNames in scalaxb,
        packageDir in scalaxb,
        classPrefix in scalaxb,
        paramPrefix in scalaxb,
        wrapContents in scalaxb,
        generateRuntime in scalaxb,
        chunkSize in scalaxb,
        protocolFileName in scalaxb,
        protocolPackageName in scalaxb) { (pkg, pkgdir, cpre, ppre, w, rt, cs, pfn, ppn) =>
      sc.Config(packageNames = pkg,
        packageDir = pkgdir,
        classPrefix = cpre,
        paramPrefix = ppre,
        wrappedComplexTypes = w.toList,
        generateRuntime = rt,
        sequenceChunkSize = cs,
        protocolFileName = pfn,
        protocolPackageName = ppn
      ) },
    logLevel in scalaxb <<= logLevel?? Level.Info
  )
}
