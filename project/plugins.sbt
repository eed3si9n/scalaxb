resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

// comment this out for production otherwise chicken-egg will entail.
// addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "0.6.8-SNAPSHOT")

resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.2")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.1.2")

addSbtPlugin("com.eed3si9n" % "sbt-scalashim" % "0.2.2")

resolvers += Resolver.url("sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

// addSbtPlugin("net.virtual-void" % "sbt-cross-building" % "0.7.0")
