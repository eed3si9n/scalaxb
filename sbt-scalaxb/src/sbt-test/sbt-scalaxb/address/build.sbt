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

val scalaVersions = Seq(
  "2.10.7",
  "2.11.12",
  "2.12.20",
  "2.13.16",
  "3.6.3",
)

lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    crossScalaVersions := {
      if (scala.util.Properties.javaVersion.startsWith("1.")) scalaVersions
      else scalaVersions.filter(v => !v.startsWith("2.10") && !v.startsWith("2.11"))
    },
    name := "mavenxsd",
    Compile / scalaxb / scalaxbAutoPackages := true,
    Compile / scalaxb / scalaxbGenerateMutable := true,
    libraryDependencies ++= scalaXml.value ++ scalaParser.value ++ Seq(jaxbApi),
    Compile / scalacOptions ++= (scalaBinaryVersion.value match {
      case "2.13"          => Seq("-deprecation", "-Werror")
      case "2.12"          => Seq("-deprecation", "-feature", "-Xfatal-warnings")
      case "2.11" | "2.10" => Seq("-deprecation", "-language:existentials", "-Xfatal-warnings")
      case _               => Seq("-deprecation", "-Werror", "-source", "3.0")
    }),
  )
