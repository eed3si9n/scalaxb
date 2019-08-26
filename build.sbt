import Dependencies._
import Common._

ThisBuild / version := "1.7.3-SNAPSHOT"
ThisBuild / organization := "org.scalaxb"
ThisBuild / homepage := Option(url("http://scalaxb.org"))
ThisBuild / licenses := List("MIT License" -> url("https://github.com/eed3si9n/scalaxb/blob/master/LICENSE"))
ThisBuild / description := """scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and wsdl."""
ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { x => false }
ThisBuild / scmInfo := Option(ScmInfo(url("https://github.com/eed3si9n/scalaxb"), "scm:git@github.com:eed3si9n/scalaxb.git"))
ThisBuild / developers := List(
  Developer(
    id    = "eed3si9n",
    name  = "Eugene Yokota",
    email = "@eed3si9n",
    url   = url("http://eed3si9n.com")
  )
)

lazy val commonSettings = Seq(
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions", "-language:postfixOps"),
    parallelExecution in Test := false,
    resolvers += Resolver.typesafeIvyRepo("releases")
  ) ++ sonatypeSettings

lazy val root = (project in file(".")).
  aggregate(app, integration, scalaxbPlugin).
  settings(
    scalaVersion := scala211,
    publish / skip := true,
    crossScalaVersions := Nil,
  )

lazy val app = (project in file("cli")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(codegenSettings: _*).
  settings(
    name := "scalaxb",
    crossScalaVersions := Seq(scala212, scala211, scala210),
    scalaVersion := scala211,
    resolvers += sbtResolver.value,
    libraryDependencies ++= appDependencies(scalaVersion.value),
    scalacOptions := {
      val prev = scalacOptions.value
      if (scalaVersion.value != scala210) {
        prev :+ "-Xfatal-warnings"
      }
      else prev
    },

    mainClass          in assembly := Some("scalaxb.compiler.Main"),
    assemblyOption     in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(sbtassembly.AssemblyPlugin.defaultShellScript)),
    assemblyOutputPath in assembly := file(s"./${name.value}-${version.value}"),
  )

lazy val integration = (project in file("integration")).
  settings(commonSettings: _*).
  settings(
    crossScalaVersions := Seq(scala211),
    scalaVersion := scala211,
    publishArtifact := false,
    libraryDependencies ++= integrationDependencies(scalaVersion.value),
    // fork in test := true,
    // javaOptions in test ++= Seq("-Xmx2G", "-XX:MaxPermSize=512M")
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument("sequential"),
    publish / skip := true,
  ).
  dependsOn(app)

lazy val scalaxbPlugin = (project in file("sbt-scalaxb")).
  enablePlugins(SbtPlugin).
  settings(commonSettings: _*).
  settings(
    name := "sbt-scalaxb",
    description := """sbt plugin to run scalaxb""",
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false,
    scripted := scripted.dependsOn(publishLocal in app).evaluated
  ).
  dependsOn(app)
