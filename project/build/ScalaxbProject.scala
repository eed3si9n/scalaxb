import sbt._

class ScalaxbProject(info: ProjectInfo) extends ParentProject(info) with posterous.Publish {
  lazy val cli = project("cli", "scalaxb cli", new CliProject(_))
  
  class CliProject(info: ProjectInfo) extends DefaultProject(info) with ScalaBazaarTask {
    override def description = "XML data binding tool for Scala."
    override def testCompileOptions = super.testCompileOptions ++ Seq(CompileOption("-no-specialization"))
  }
  
  val specsVersion = crossScalaVersionString match {
    case "2.8.0" => "1.6.5"
    case _ => "1.6.6"
  }
  
  val specs = "org.scala-tools.testing" % ("specs_" + crossScalaVersionString) % specsVersion % "test"
  val junit = "junit" % "junit" % "4.7" % "test"
  
  val scalaToolsSnapshots = "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  // val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
