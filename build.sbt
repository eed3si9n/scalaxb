import Dependencies._
import Common._

lazy val commonSettings = Seq(
    version in ThisBuild := "1.5.2",
    organization in ThisBuild := "org.scalaxb",
    homepage in ThisBuild := Some(url("http://scalaxb.org")),
    licenses in ThisBuild := Seq("MIT License" -> url("https://github.com/eed3si9n/scalaxb/blob/master/LICENSE")),
    description in ThisBuild := """scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and wsdl.""",
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions", "-language:postfixOps"),
    parallelExecution in Test := false,
    resolvers += Resolver.typesafeIvyRepo("releases")
  ) ++ sonatypeSettings

lazy val root = (project in file(".")).
  enablePlugins(NoPublish).
  disablePlugins(ScriptedPlugin).
  aggregate(app, integration, scalaxbPlugin).
  settings(
    scalaVersion := scala211
  )

lazy val app = (project in file("cli")).
  enablePlugins(BuildInfoPlugin).
  disablePlugins(ScriptedPlugin).
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
  disablePlugins(ScriptedPlugin).
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
  ).
  dependsOn(app)

lazy val scalaxbPlugin = (project in file("sbt-scalaxb")).
  enablePlugins(ScriptedPlugin).
  settings(commonSettings: _*).
  settings(
    sbtPlugin := true,
    name := "sbt-scalaxb",
    description := """sbt plugin to run scalaxb""",
    crossSbtVersions := Vector("0.13.16", "1.0.0"),
    scriptedLaunchOpts := { scriptedLaunchOpts.value ++
      Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false,
    scripted := scripted.dependsOn(publishLocal in app).evaluated
  ).
  dependsOn(app)
