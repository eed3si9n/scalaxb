import sbt._

class ScalaxbProject(info: ProjectInfo) extends DefaultProject(info) with posterous.Publish
    with ScalaBazaarTask {
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5-SNAPSHOT" % "test"
  val junit = "junit" % "junit" % "4.7" % "test"
  
  val scalaToolsSnapshots = "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  
  override def description = "XML data binding tool for Scala."
  
  override def testCompileOptions = super.testCompileOptions ++ Seq(CompileOption("-no-specialization"))

  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  //val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
