package scalaxb

import sbt._
import sbt.{FileUtilities => FU}

trait ScalaxbPlugin extends DefaultProject {
  def generatedPackageName: String = "generated"
  def generatedClassPrefix: Option[String] = None
  def generatedParamPrefix: Option[String] = None
  
  def rootPath = path(".")
  def xsdSourcePath = rootPath / "src" / "main" / "xsd"
  def scalaxbOutputPath = outputRootPath / "src_generated"
  override def mainSourceRoots = super.mainSourceRoots +++ (scalaxbOutputPath##) 
    
  lazy val compileXsd = compileXsdAction(scalaxbOutputPath,
    Seq("-p", generatedPackageName) ++
    (generatedClassPrefix map { Seq("--class-prefix", _) } getOrElse {Nil}) ++
    (generatedParamPrefix map { Seq("--param-prefix", _) } getOrElse {Nil}),
    xsdSourcePath ** "*.xsd")

  def compileXsdAction(out: Path, args: Seq[String], xsds: PathFinder) =
    task {
      FU.clean(out, log)
      FU.createDirectory(out, log)
    } && runTask(
      Some(scalaxbCompilerMain),
      scalaxbDepPath ** "*.jar",
      Seq("-d", out.toString) ++ args ++ xsds.getPaths
    )
  
  val scalaxbCompilerMain = "scalaxb.compiler.Main"
  val scalaxbDepPath = info.parent match {
    case Some(p) => p.info.pluginsManagedDependencyPath
    case _ => info.pluginsManagedDependencyPath
  }
}
