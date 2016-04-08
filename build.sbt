import Dependencies._
import Common._

lazy val commonSettings = Seq(
    version in ThisBuild := "1.4.2-SNAPSHOT",
    organization in ThisBuild := "org.scalaxb",
    homepage in ThisBuild := Some(url("http://scalaxb.org")),
    licenses in ThisBuild := Seq("MIT License" -> url("https://github.com/eed3si9n/scalaxb/blob/master/LICENSE")),
    description in ThisBuild := """scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and wsdl.""",
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions", "-language:postfixOps"),
    parallelExecution in Test := false,
    resolvers += Resolver.typesafeIvyRepo("releases")
  ) ++ sonatypeSettings ++ lsSettings

lazy val root = (project in file(".")).
  aggregate(app, integration, scalaxbPlugin).
  settings(
    scalaVersion := scala211,
    publishArtifact := false
  )

lazy val app = (project in file("cli")).
  settings(commonSettings).
  settings(codegenSettings).
  settings(
    name := "scalaxb",
    crossScalaVersions := Seq(scala211, scala210),
    scalaVersion := scala211,
    resolvers <+= sbtResolver,
    libraryDependencies ++= appDependencies(scalaVersion.value),
    scalacOptions := {
      val prev = scalacOptions.value
      if (scalaVersion.value == scala211) {
        prev :+ "-Xfatal-warnings"
      }
      else prev
    }
  )

lazy val integration = (project in file("integration")).
  settings(commonSettings).
  settings(
    crossScalaVersions := Seq(scala211),
    scalaVersion := scala211,
    publishArtifact := false,
    libraryDependencies ++= integrationDependencies(scalaVersion.value)
    // fork in test := true,
    // javaOptions in test ++= Seq("-Xmx2G", "-XX:MaxPermSize=512M")
  ).
  dependsOn(app)

lazy val scalaxbPlugin = (project in file("sbt-scalaxb")).
  settings(commonSettings).
  settings(
    crossScalaVersions := Seq(scala210),
    scalaVersion := scala210,
    sbtPlugin := true,
    name := "sbt-scalaxb",
    // sbtVersion in Global := "0.12.4",
    // scalaVersion in Global := "2.9.2",
    description := """sbt plugin to run scalaxb""",
    ScriptedPlugin.scriptedSettings,
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false,
    scripted <<= scripted.dependsOn(publishLocal in app)
  ).
  dependsOn(app)
