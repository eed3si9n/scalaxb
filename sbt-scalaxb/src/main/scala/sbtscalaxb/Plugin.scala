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
  }

  object ScalaxbCompile {
    def apply(sources: Seq[File], packageName: String, outdir: File): Seq[File] =
      apply(sources, sc.Config(packageNames = Map(None -> Some(packageName))), outdir, false)

    def apply(sources: Seq[File], config: sc.Config, outdir: File, verbose: Boolean = false): Seq[File] =
      sources.headOption map { src =>
        import sc._
        sc.Log.configureLogger(verbose)
        val module = Module.moduleByFileName(src)
        module.processFiles(sources, config.copy(outdir = outdir))
      } getOrElse {Nil}
  }

  lazy val scalaxbSettings: Seq[Project.Setting[_]] = inConfig(Compile)(baseScalaxbSettings)
  lazy val baseScalaxbSettings: Seq[Project.Setting[_]] = Seq(
    scalaxb <<= generate in scalaxb,
    generate in scalaxb <<= (sources in scalaxb, scalaxbConfig in scalaxb,
        sourceManaged in scalaxb, logLevel in scalaxb) map { (sources, config, outdir, ll) =>
      ScalaxbCompile(sources, config, outdir, ll == Level.Debug) },
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
    combinedPackageNames in scalaxb <<= (packageName in scalaxb, packageNames in scalaxb) { (x, xs) =>
      (xs map { case (k, v) => ((Some(k.toString): Option[String]), Some(v)) }) updated (None, Some(x)) },
    scalaxbConfig in scalaxb <<= Project.app((combinedPackageNames in scalaxb) :^:
        (packageDir in scalaxb) :^:
        (classPrefix in scalaxb) :^:
        (paramPrefix in scalaxb) :^:
        (attributePrefix in scalaxb) :^:
        (prependFamily in scalaxb) :^:
        (wrapContents in scalaxb) :^:
        (generateRuntime in scalaxb) :^:
        (contentsSizeLimit in scalaxb) :^:
        (chunkSize in scalaxb) :^:
        (protocolFileName in scalaxb) :^:
        (protocolPackageName in scalaxb) :^:
        (laxAny in scalaxb) :^: KNil) {
          case pkg :+: pkgdir :+: cpre :+: ppre :+: apre :+: pf :+: 
              w :+: rt :+: csl :+: cs :+: pfn :+: ppn :+: la :+: HNil =>
            ScConfig(packageNames = pkg,
              packageDir = pkgdir,
              classPrefix = cpre,
              paramPrefix = ppre,
              attributePrefix = apre,
              wrappedComplexTypes = w.toList,
              protocolFileName = pfn,
              protocolPackageName = ppn,
              generateRuntime = rt,
              contentsSizeLimit = csl,
              sequenceChunkSize = cs,
              prependFamilyName = pf,
              laxAny = la
            )
          },
    logLevel in scalaxb <<= logLevel?? Level.Info
  )
}
