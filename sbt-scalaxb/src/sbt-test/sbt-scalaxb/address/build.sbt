val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    scalaVersion := "2.12.7",
    name := "mavenxsd",
    scalaxbAutoPackages in (Compile, scalaxb) := true,
    scalaxbGenerateMutable in (Compile, scalaxb) := true,
    libraryDependencies ++= Seq(scalaXml, scalaParser)
  )
