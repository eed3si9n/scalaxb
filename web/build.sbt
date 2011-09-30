libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-filter" % "0.4.0",
  "net.databinder" %% "unfiltered-uploads" % "0.4.0",
  "javax.servlet" % "servlet-api" % "2.3" % "provided"
)

crossScalaVersions := Seq("2.9.0-1", "2.8.1")

scalaVersion := "2.9.0-1"

publish := {}

publishLocal := {}
