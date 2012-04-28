scalaxb
=======

scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and
Web Services Description Language (wsdl) as the input file.

From schema documents scalaxb will generate Scala source files containing
case classes to represent the data and typeclass instances to turn XML documents into an object,
and the object back to XML.

Status
------

The latest is 0.7.0. Some things may not work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

Modules
-------

There are currently four ways of running scalaxb:

- command line app `scalaxb`
- sbt plugin sbt-scalaxb
- maven plugin mvn-scalaxb
- web API scalaxb-heroku hosted on heroku

### sbt-scalaxb for sbt 0.11.2

To call scalaxb from sbt 0.11.2, put this in your `project/plugins.sbt`:

    resolvers ++= Seq(
      "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
      "repo.codahale.com" at "http://repo.codahale.com")

    addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "X.X")
    
and this in `build.sbt`:

    seq(scalaxbSettings: _*)

    packageName in scalaxb in Compile := "xxx"

    sourceGenerators in Compile <+= scalaxb in Compile

### sbt-scalaxb for sbt 0.10.1

To call scalaxb from sbt 0.10.1, put this in your `project/plugins/build.sbt`:

    resolvers ++= Seq(
      "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
      "repo.codahale.com" at "http://repo.codahale.com")

    libraryDependencies <+= (sbtVersion) { sv => "org.scalaxb" %% "sbt-scalaxb" % ("sbt" + sv + "_X.X") }

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

  [1]: https://github.com/eed3si9n/scalaxb/blob/master/INSTALL.md
  [2]: http://scalaxb.org/mvn-scalaxb
  [3]: http://scalaxb.org/issue-reporting-guideline
