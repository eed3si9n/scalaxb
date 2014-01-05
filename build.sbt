import Dependencies._
import Common._

val commonSettings = Seq(
    version := "1.1.3-SNAPSHOT",
    organization := "org.scalaxb",
    homepage := Some(url("http://scalaxb.org")),
    licenses := Seq("MIT License" -> url("https://github.com/eed3si9n/scalaxb/blob/master/LICENSE")),
    description := """scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and wsdl.""",
    scalaVersion := "2.10.3",
    crossScalaVersions := Seq("2.10.3", "2.9.2", "2.9.1"),
    scalacOptions := Seq("-deprecation", "-unchecked"),
    parallelExecution in Test := false
  ) ++ sonatypeSettings ++ lsSettings

val app = (project in file("cli")).
  settings(commonSettings: _*).
  settings(codegenSettings: _*).
  settings(
    name := "scalaxb",
    libraryDependencies ++= appDependencies
  )

val integration = (project in file("integration")).
  settings(commonSettings: _*).
  settings(
    publishArtifact := false,
    libraryDependencies <<= scalaVersion(integrationDependencies)
    // fork in test := true,
    // javaOptions in test ++= Seq("-Xmx2G", "-XX:MaxPermSize=512M")
  ).
  dependsOn(app)

val scalaxbPlugin = (project in file("sbt-scalaxb")).
  settings(commonSettings: _*).
  settings(
    sbtPlugin := true,
    name := "sbt-scalaxb",
    description := """sbt plugin to run scalaxb""",
    sbtVersion in Global := "0.12.4",
    scalaVersion in Global := "2.9.2" 
  ).
  dependsOn(app)
