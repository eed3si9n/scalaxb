scalacOptions += "-no-specialization"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-http" % "0.8.2" % "test"  
)

parallelExecution in Test := false
