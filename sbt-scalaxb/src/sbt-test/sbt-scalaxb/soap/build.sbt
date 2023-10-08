lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
lazy val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
lazy val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.0"
lazy val http4sVersion = "0.23.18"
lazy val emberClient= "org.http4s" %% "http4s-ember-client" % http4sVersion

ThisBuild / organization  := "com.example"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / scalaxbPackageName := "generated"
ThisBuild / scalaxbGenerateDispatchClient := false
ThisBuild / scalaxbGenerateHttp4sClient := true

lazy val root = (project in file("."))
  .enablePlugins(ScalaxbPlugin)
  .settings(
    name := "soap",
    libraryDependencies ++= Seq(scalaXml, scalaParser, jaxbApi, emberClient),
  )
