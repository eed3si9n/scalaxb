package sbtscalaxb

import sbt._
import Keys._
import scalaxb.{compiler => sc}
import scalaxb.compiler.{Config => ScConfig}
import sc.ConfigEntry._
import scalaxb.BuildInfo

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
