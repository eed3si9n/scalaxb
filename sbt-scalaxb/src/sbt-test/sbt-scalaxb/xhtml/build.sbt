lazy val scala3BleedingEdge = "3.5.0"
lazy val scala3_4 = "3.4.3"
lazy val scala3Lts = "3.3.6"
lazy val scalaXml = Def.setting(
  scalaBinaryVersion.value match {
    case _ => Seq("org.scala-lang.modules" %% "scala-xml" % "2.2.0")
  }
)
lazy val scalaParser = Def.setting(
  scalaBinaryVersion.value match {
    case _ => Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0")
  }
)
lazy val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.1"
lazy val verify = "com.eed3si9n.verify" %% "verify" % "1.0.0"

ThisBuild / scalaVersion := scala3BleedingEdge
ThisBuild / scalaxbPackageName := "xhtml"

lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    crossScalaVersions := Seq(scala3Lts, scala3_4, scala3BleedingEdge),
    name := "root",
    Compile / scalacOptions ++= List("-Xfatal-warnings"),
    testFrameworks += new TestFramework("verify.runner.Framework"),
    libraryDependencies ++= scalaXml.value ++ scalaParser.value ++ Seq(jaxbApi),
    libraryDependencies += verify % Test,
  )
