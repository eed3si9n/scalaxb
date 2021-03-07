import Dependencies._
import Common._

ThisBuild / version := "1.8.2-SNAPSHOT"
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
    Test / parallelExecution := false,
    resolvers += Resolver.typesafeIvyRepo("releases"),
    // Adds a `src/test/scala-2.13+` source directory for Scala 2.13 and newer
    // and a `src/test/scala-2.13-` source directory for Scala version older than 2.13
    Test / unmanagedSourceDirectories += {
      val sourceDir = (Test / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
        case _                       => sourceDir / "scala-2.13-"
      }
    }
  ) ++ sonatypeSettings

lazy val root = (project in file("."))
  .aggregate(app, integration, scalaxbPlugin)
  .settings(nocomma {
    scalaVersion := scala211
    publish / skip := true
    crossScalaVersions := Nil
    commands += Command.command("release") { state =>
      "clean" ::
        "+app/publishSigned" ::
        "++2.10.7!;scalaxbPlugin/publishSigned" ::
        "++2.12.12!;scalaxbPlugin/publishSigned" ::
        state
    }
  })

lazy val app = (project in file("cli"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(codegenSettings)
  .settings(nocomma {
    name := "scalaxb"
    crossScalaVersions := Seq(scala213, scala212, scala211, scala210)
    scalaVersion := scala212
    resolvers += sbtResolver.value
    libraryDependencies ++= appDependencies(scalaVersion.value)
    scalacOptions := {
      val prev = scalacOptions.value
      if (scalaVersion.value == scala212) {
        prev :+ "-Xfatal-warnings"
      }
      else prev
    }
    assembly / mainClass           := Some("scalaxb.compiler.Main")
    assembly / assemblyOption      := (assembly / assemblyOption).value.copy(prependShellScript = Some(sbtassembly.AssemblyPlugin.defaultShellScript))
    assembly / assemblyOutputPath  := file(s"./${name.value}-${version.value}")
  })

lazy val integration = (project in file("integration"))
  .dependsOn(app)
  .settings(commonSettings)
  .settings(nocomma {
    crossScalaVersions := Seq(scala212, scala213)
    scalaVersion := scala212
    publishArtifact := false
    libraryDependencies ++= integrationDependencies(scalaVersion.value)
    // fork in test := true,
    // javaOptions in test ++= Seq("-Xmx2G", "-XX:MaxPermSize=512M")
    Test / parallelExecution := false
    Test / testOptions += Tests.Argument("sequential")
    publish / skip := true
  })

lazy val scalaxbPlugin = (project in file("sbt-scalaxb"))
  .enablePlugins(SbtPlugin)
  .dependsOn(app)
  .settings(commonSettings)
  .settings(nocomma {
    name := "sbt-scalaxb"
    description := """sbt plugin to run scalaxb"""
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.10" => "0.13.18"
        case "2.12" => "1.2.8" // set minimum sbt version
      }
    }
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
    }
    scriptedBufferLog := false
    scripted := scripted.dependsOn(app / publishLocal).evaluated
  })
