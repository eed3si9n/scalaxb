import sbt._

class ScalaxbProject(info: ProjectInfo) extends DefaultProject(info)
    with ScalaBazaarTask {
  val specs = "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.2" % "test"
  val junit = "junit" % "junit" % "4.7" % "test"
  
  override def description = "XML data binding tool for Scala."
}
