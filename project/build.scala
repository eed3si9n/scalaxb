import sbt._

object Builds extends Build {
  import Keys._
  
	lazy val root = Project("root", file("."))
	lazy val cli = Project("scalaxb", file("cli"))
	lazy val integration = Project("integration", file("integration")) dependsOn(cli % "test")  
  lazy val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb"))
  
  override lazy val settings = super.settings ++ Seq(
    version := "0.6.2-SNAPSHOT",
    organization := "org.scalaxb",
    scalaVersion := "2.9.0-1",
    crossScalaVersions := Seq("2.9.0-1", "2.8.1")
  )
}
