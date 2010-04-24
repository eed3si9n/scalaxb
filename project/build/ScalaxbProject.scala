import sbt._

class ScalaxbProject(info: ProjectInfo) extends DefaultProject(info) with posterous.Publish
    with ScalaBazaarTask {
  val specs = "org.scala-tools.testing" % "specs_2.8.0.RC1" % "1.6.5-SNAPSHOT" % "test"
  val junit = "junit" % "junit" % "4.7" % "test"
  
  val scalaToolsSnapshots = "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  
  override def description = "XML data binding tool for Scala."
  
  override def testCompileOptions = super.testCompileOptions ++ Seq(CompileOption("-no-specialization"))
}
