libraryDependencies += "com.github.scopt" %% "scopt" % "1.1.2"

libraryDependencies += "org.scala-tools.sbt" % "launcher-interface" % "0.7.4" % "provided" from (
  "http://databinder.net/repo/org.scala-tools.sbt/launcher-interface/0.7.4/jars/launcher-interface.jar")

unmanagedSourceDirectories in Compile <+= baseDirectory( _ / "src_generated" )

sourceGenerators in Compile <+= (sourceManaged in Compile, version) map { (dir, version) =>
  val file = dir / "version.scala"
  IO.write(file, """package scalaxb
trait Version { val version = "%s" }
""".format(version))
  Seq(file)
}

seq(sbtassembly.Plugin.assemblySettings: _*)
