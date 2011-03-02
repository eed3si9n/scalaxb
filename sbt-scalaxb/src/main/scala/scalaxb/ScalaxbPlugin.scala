package scalaxb

import sbt._
import sbt.{FileUtilities => FU}

trait ScalaxbPlugin extends DefaultProject {
  lazy val defaultPackageName = "generated"
  
  lazy val rootPath = path(".")
  lazy val scalaSourcePath = rootPath / "src" / "main" / "scala"
  lazy val xsdSourcePath = rootPath / "src" / "main" / "xsd"
  lazy val scalaxbOutputPath = scalaSourcePath / defaultPackageName
  
  lazy val compileXsd = compileXsdAction(scalaxbOutputPath,
    defaultPackageName, xsdSourcePath ** "*.xsd")
  def compileXsdAction(out: Path, pkg: String, xsds: PathFinder) =
    task {
      FU.clean(out, log)
      FU.createDirectory(out, log)
    } && runTask(
      Some(scalaxbCompilerMain),
      scalaxbDepPath ** "*.jar",
      Seq("-d", out.toString, "-p", pkg) ++ xsds.getPaths
    )
  
  val scalaxbCompilerMain = "scalaxb.compiler.Main"
  val scalaxbDepPath = info.parent match {
    case Some(p) => p.info.pluginsManagedDependencyPath
    case _ => info.pluginsManagedDependencyPath
  }
}
