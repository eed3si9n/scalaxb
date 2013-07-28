import Dependencies._
import Common._

val commonSettings = Seq(
    version := "1.1.2-SNAPSHOT",
    organization := "org.scalaxb",
    homepage := Some(url("http://scalaxb.org")),
    licenses := Seq("MIT License" -> url("https://github.com/eed3si9n/scalaxb/blob/master/LICENSE")),
    description := """scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and wsdl.""",
    scalaVersion := "2.10.2",
    crossScalaVersions := Seq("2.10.2", "2.9.2", "2.9.1"),
    scalacOptions := Seq("-deprecation", "-unchecked"),
    parallelExecution in Test := false
  ) ++ sonatypeSettings

val app = Project("app", file("cli")).
  settings(commonSettings: _*).
  settings(codegenSettings: _*).
  settings(
    name := "scalaxb",
    libraryDependencies ++= appDependencies
  )

val integration = Project("integration", file("integration")).
  settings(commonSettings: _*).
  settings(
    publishArtifact := false,
    libraryDependencies ++= integrationDependencies(scalaVersion.value)
  ).
  dependsOn(app)

val scalaxbPlugin = Project("sbt-scalaxb", file("sbt-scalaxb")).
  settings(commonSettings: _*).
  settings(
    sbtPlugin := true,
    description := """sbt plugin to run scalaxb"""
  ).
  dependsOn(app)

