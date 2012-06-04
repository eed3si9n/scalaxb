resolvers += ("ScalaTools Snapshots" at "http://scala-tools.org/repo-snapshots/")

// comment this out for production otherwise chicken-egg will entail.
// addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "0.6.8-SNAPSHOT")

// addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.6")

// libraryDependencies <++= (sbtVersion) { sv =>
//  Seq("org.scala-tools.sbt" %% "scripted-plugin" % sv,
//      "org.scala-tools.sbt" %% "scripted-sbt" % sv)
// }

// resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"
//
// addSbtPlugin("com.github.siasia" % "xsbt-web-plugin" % "0.1.2")
//
// addSbtPlugin("com.eed3si9n" % "sbt-appengine" % "0.3.0")

resolvers ++= Seq(
  "less is" at "http://repo.lessis.me")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.1.2")

addSbtPlugin("com.eed3si9n" % "sbt-scalashim" % "0.2.2")

resolvers += Resolver.url("sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
