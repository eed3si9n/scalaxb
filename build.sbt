name := "scalaxb"

publishMavenStyle := true

resolvers += ScalaToolsReleases

resolvers += ScalaToolsSnapshots

publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/")

// publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
