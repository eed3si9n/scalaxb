resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("com.lucidchart" % "sbt-cross" % "1.0")

// comment this out for production otherwise chicken-egg will entail.
// addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "1.1.1")
