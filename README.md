scalaxb
=======

![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.scalaxb/scalaxb_2.12/badge.svg)

scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and
Web Services Description Language (wsdl) as the input file.

From schema documents scalaxb will generate Scala source files containing
case classes to represent the data and typeclass instances to turn XML documents into an object,
and the object back to XML.

Modules
-------

There are currently four ways of running scalaxb:

- command line app `scalaxb`
- sbt plugin sbt-scalaxb
- maven plugin mvn-scalaxb
- web API scalaxb-heroku hosted on heroku

### sbt-scalaxb

To call scalaxb from sbt 1.x and sbt 0.13.x, put this in your `project/scalaxb.sbt`:

    resolvers += Resolver.sonatypeRepo("public")
    addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "X.X.X")

and this in `build.sbt`:

```scala
lazy val dispatchVersion = "1.1.3"
lazy val dispatch = "org.dispatchhttp" %% "dispatch-core" % dispatchVersion
lazy val jaxbApi = "javax.xml.bind" % "jaxb-api" % "2.3.0"
lazy val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
lazy val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    name := "foo-project",
    Compile / scalaxb / scalaxbPackageName := "generated",
    // Compile / scalaxb / scalaxbAutoPackages := true,
    Compile / scalaxb / scalaxbDispatchVersion := dispatchVersion,
    libraryDependencies ++= Seq(dispatch, jaxbApi, scalaParser, scalaXml)
  )
```

### command line app scalaxb

See [INSTALL.md][1].

### mvn-scalaxb

See [mvn-scalaxb][2].

Documents
---------

Further info is available at [scalaxb.org](http://scalaxb.org/).

Bug Reporting
-------------

If you're having problem with scalaxb, please take a moment and read [issue reporting guideline][3].

Licensing
---------

It's the MIT License. See the file called LICENSE.

Contacts
--------

- [mailing list](http://groups.google.com/group/scalaxb)
- [@scalaxb](http://twitter.com/scalaxb)

  [1]: https://github.com/eed3si9n/scalaxb/blob/HEAD/INSTALL.md
  [2]: http://scalaxb.org/mvn-scalaxb
  [3]: http://scalaxb.org/issue-reporting-guideline
