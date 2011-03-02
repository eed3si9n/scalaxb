package sbt_scalaxb

import sbt._
import sbt.{FileUtilities => FU}

trait SbtScalaxbPlugin extends DefaultProject {
  val scalaxbCompilerMain = "scalaxb.compiler.Main"
  val scalaxbDepPath = info.parent match {
    case Some(p) => p.info.pluginsManagedDependencyPath
    case _ => info.pluginsManagedDependencyPath
  }

  def compileXsdAction(out: Path, pkg: String, xsds: PathFinder) = {
    task {
      FU.clean(out, log)
      FU.createDirectory(out, log)
    } && runTask(
      Some(scalaxbCompilerMain),
      scalaxbDepPath ** "*.jar",
      Seq("-d", out.toString, "-p", pkg) ++ xsds.getPaths
    )
  }
}
