name := "scalaxb"

version := "0.6.2-SNAPSHOT"

organization := "org.scalaxb"

scalaVersion := "2.8.1"

// crossScalaVersions := Seq("2.8.1", "2.9.0-1")

publishMavenStyle := true

resolvers += ScalaToolsReleases

resolvers += ScalaToolsSnapshots

// publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/")

publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
