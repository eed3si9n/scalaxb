scalacOptions += "-no-specialization"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

libraryDependencies ++= Seq(
  "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test",
  "net.databinder" %% "dispatch-http" % "0.8.2" % "test"  
)

parallelExecution in Test := false
