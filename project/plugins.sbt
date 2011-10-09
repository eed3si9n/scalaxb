// addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.6")

resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

addSbtPlugin("com.github.siasia" % "xsbt-web-plugin" % "0.1.2")

addSbtPlugin("com.eed3si9n" % "sbt-appengine" % "0.3.0")

libraryDependencies <+= (sbtVersion) { sv =>
  "org.scala-tools.sbt" %% "scripted-plugin" % sv
}

// uncomment this during development, otherwise chicken-egg will entail.
addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "0.6.5")
