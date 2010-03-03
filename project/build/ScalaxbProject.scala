import sbt._

class ScalaxbProject(info: ProjectInfo) extends DefaultProject(info)
    with ScalaScriptTask with ScalaBazaarTask {
  val specs = "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.2" % "test"
  
  def scriptPath = outputBinPath / name
  def description = "XML data binding tool for Scala."
  
  lazy val sbaz = sbazTask(Nil, Some(description)).dependsOn(`package`, doc, scalascript)
  lazy val scalascript = scalascriptTask(scriptPath.asFile, "scalaxb.Main", Nil, Nil,
    "-Xmx256M -Xms16M", "")
}
