import sbt._

object Builds extends Build
{
	lazy val root = Project("root", file("."))
	// lazy val cli = Project("scalaxb", file("cli"), delegates = root :: Nil)  
  lazy val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb"), delegates = root :: Nil)
}
