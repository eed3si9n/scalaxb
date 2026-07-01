lazy val scala3BleedingEdge = "3.5.0"
lazy val scala3Lts = "3.3.8"

lazy val scalaXml = Def.setting(
  scalaBinaryVersion.value match {
    case "2.10" =>
      Nil
    case "2.11" | "2.12" =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.1.1")
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "2.2.0")
  }
)
lazy val scalaParser = Def.setting(
  scalaBinaryVersion.value match {
    case "2.10" =>
      Nil
    case "2.11" | "2.12" =>
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1")
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0")
  }
)
lazy val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.0"
lazy val http4sVersion = "0.23.18"
lazy val emberClient= "org.http4s" %% "http4s-ember-client" % http4sVersion

ThisBuild / organization  := "com.example"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.0"
ThisBuild / scalaxbPackageName := "generated"
ThisBuild / scalaxbGenerateDispatchClient := false
ThisBuild / scalaxbGenerateHttp4sClient := true

lazy val root = (project in file("."))
  .enablePlugins(ScalaxbPlugin)
  .settings(
    crossScalaVersions := Seq(scala3Lts, scala3BleedingEdge),
    name := "soap",
    Compile / scalacOptions ++= List("-Xfatal-warnings"),
    libraryDependencies ++= scalaXml.value ++ scalaParser.value ++ Seq(jaxbApi, emberClient),
  )
