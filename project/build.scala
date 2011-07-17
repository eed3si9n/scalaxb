import sbt._

object Builds extends Build {
  import Keys._
  import sbtappengine.AppenginePlugin
  
	lazy val root = Project("root", file(".")) aggregate(cli, scalaxbPlugin)
	lazy val cli = Project("scalaxb", file("cli"))
	lazy val integration = Project("integration", file("integration")) dependsOn(cli % "test")  
  lazy val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb")) dependsOn(cli)
  lazy val appengine = Project("web", file("web"),
    settings = Defaults.defaultSettings ++ AppenginePlugin.webSettings) dependsOn(cli)
  
  override lazy val settings = super.settings ++ Seq(
    version := "0.6.2-SNAPSHOT",
    organization := "org.scalaxb",
    scalaVersion := "2.9.0-1",
    crossScalaVersions := Seq("2.9.0-1", "2.8.1"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "1.4" % "test"
    ),
    publishArtifact in (Compile, packageBin) := true,
    publishArtifact in (Test, packageBin) := false,
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in (Compile, packageSrc) := false,
    resolvers += ScalaToolsSnapshots,
    publishTo <<= version { (v: String) =>
      val nexus = "http://nexus.scala-tools.org/content/repositories/"
      if(v endsWith "-SNAPSHOT") Some("Scala Tools Nexus" at nexus + "snapshots/")
      else Some("Scala Tools Nexus" at nexus + "releases/")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )
}
