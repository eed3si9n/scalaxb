resolvers ++= Seq(
  "Web plugin repo" at "http://siasia.github.com/maven2",
  Resolver.url("Typesafe repository", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))(Resolver.defaultIvyPatterns)
)

libraryDependencies <+= (sbtVersion) { sv => "com.eed3si9n" %% "sbt-assembly" % ("sbt" + sv + "_0.6") }

libraryDependencies <+= (sbtVersion) { sv => "com.eed3si9n" %% "sbt-appengine" % ("sbt" + sv + "_0.2") }

libraryDependencies <+= sbtVersion("com.github.siasia" %% "xsbt-proguard-plugin" % _)

// resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

// libraryDependencies += "com.github.mpeltonen" %% "sbt-idea" % "0.10.0"
