sbtPlugin := true

scalaVersion := "2.8.1"

crossScalaVersions := Seq("2.8.1")

version <<= (sbtVersion, version) { (sv, nv) => "sbt" + sv + "_" + nv }

publishMavenStyle := true
