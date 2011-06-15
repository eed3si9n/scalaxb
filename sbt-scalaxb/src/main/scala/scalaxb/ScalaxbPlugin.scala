package scalaxb

import sbt._
import sbt.{FileUtilities => FU}
import java.net.URI

trait ScalaxbPlugin extends DefaultProject {
  def generatedPackageName: String = "generated"
  def generatedPackageNames: Map[URI, String] = Map()
  def generatedClassPrefix: Option[String] = None
  def generatedParamPrefix: Option[String] = None
  def generatePackageDir: Boolean = true
  def generateWrapContents: Seq[String] = Nil
  def generateRuntime: Boolean = true
  def generatedChunkSize: Int = 10

  def rootPath = path(".")
  def xsdSourcePath = rootPath / "src" / "main" / "xsd"
  def scalaxbOutputPath = rootPath / "src_generated"
  override def mainSourceRoots = super.mainSourceRoots +++ (scalaxbOutputPath##) 
    
  lazy val compileXsd = compileXsdAction(scalaxbOutputPath,
    Seq("-p", generatedPackageName) ++
    (generatedPackageNames.toList map { case (ns, pkg) => "-p:%s=%s" format(ns.toString, pkg) }) ++
    (if (generatePackageDir) Seq("--package-dir") else Nil) ++
    (generatedClassPrefix map { Seq("--class-prefix", _) } getOrElse {Nil}) ++
    (generatedParamPrefix map { Seq("--param-prefix", _) } getOrElse {Nil}) ++
    (generateWrapContents flatMap { Seq("--wrap-contents", _) }) ++
    (if (generateRuntime) Nil else Seq("--no-runtime") ) ++
    (if (generatedChunkSize == 10) Nil else Seq("--chunk-size", generatedChunkSize.toString) ),
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
