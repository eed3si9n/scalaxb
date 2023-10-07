val scalaXml = Def.setting(
  scalaBinaryVersion.value match {
    case "2.10" =>
      Nil
    case "2.11" | "2.12" =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.1.1")
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "2.2.0")
  }
)
val scalaParser = Def.setting(
  scalaBinaryVersion.value match {
    case "2.10" =>
      Nil
    case "2.11" | "2.12" =>
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1")
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0")
  }
)
val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.1"

lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    crossScalaVersions := Seq(
      "2.10.7",
      "2.11.12",
      "2.12.18",
      "2.13.12",
      "3.3.1",
    ),
    name := "mavenxsd",
    scalaxbAutoPackages in (Compile, scalaxb) := true,
    scalaxbGenerateMutable in (Compile, scalaxb) := true,
    libraryDependencies ++= scalaXml.value,
    libraryDependencies ++= scalaParser.value,
    libraryDependencies += jaxbApi
  )
