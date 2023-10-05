lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
lazy val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
lazy val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.0"
lazy val http4sVersion = "0.23.18"
lazy val emberClient= "org.http4s" %% "http4s-ember-client" % http4sVersion

organization in ThisBuild  := "com.example"
version in ThisBuild := "0.1.0-SNAPSHOT"
scalaVersion in ThisBuild := "2.13.12"
scalaxbPackageName in ThisBuild := "generated"
scalaxbGenerateDispatchClient in ThisBuild := false
scalaxbGenerateHttp4sClient in ThisBuild := true

lazy val root = (project in file("."))
  .enablePlugins(ScalaxbPlugin)
  .settings(
    name := "soap",
    libraryDependencies ++= Seq(scalaXml, scalaParser, jaxbApi, emberClient),
  )
