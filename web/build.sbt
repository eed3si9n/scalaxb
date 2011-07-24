resolvers ++= Seq(
  "Web plugin repo" at "http://siasia.github.com/maven2",
  Resolver.url("Typesafe repository", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))(Resolver.defaultIvyPatterns)
)

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-filter" % "0.3.4",
  "net.databinder" %% "unfiltered-uploads" % "0.3.4",
  "javax.servlet" % "servlet-api" % "2.3" % "provided"
)

publish := {}

publishLocal := {}
