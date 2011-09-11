scalacOptions += "-no-specialization"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-http" % "0.8.5" % "test"
)

parallelExecution in Test := false

publish := {}

publishLocal := {}
