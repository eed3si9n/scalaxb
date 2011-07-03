import sbt._

object Builds extends Build {
  import Keys._
  import sbtappengine.AppenginePlugin
  
	lazy val root = Project("root", file("."))
	lazy val cli = Project("scalaxb", file("cli"))
	lazy val integration = Project("integration", file("integration")) dependsOn(cli % "test")  
  lazy val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb"))
  lazy val appengine = Project("web", file("web"),
    settings = Defaults.defaultSettings ++ AppenginePlugin.webSettings) dependsOn(cli)
  
  override lazy val settings = super.settings ++ Seq(
    version := "0.6.2-SNAPSHOT",
    organization := "org.scalaxb",
    scalaVersion := "2.9.0-1",
    crossScalaVersions := Seq("2.9.0-1", "2.8.1")
  )
}
