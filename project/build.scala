import sbt._

object Builds extends Build
{
	lazy val root = Project("root", file("."))  
  lazy val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb"), delegates = root :: Nil)
}
