scalaxb
=======

[![Join the chat at https://gitter.im/eed3si9n/scalaxb](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/eed3si9n/scalaxb?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

scalaxb is an XML data-binding tool for Scala that supports W3C XML Schema (xsd) and
Web Services Description Language (wsdl) as the input file.

From schema documents scalaxb will generate Scala source files containing
case classes to represent the data and typeclass instances to turn XML documents into an object,
and the object back to XML.

Status
------

The latest is 1.5.0. Some things may not work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

Modules
-------

There are currently four ways of running scalaxb:

- command line app `scalaxb`
- sbt plugin sbt-scalaxb
- maven plugin mvn-scalaxb
- web API scalaxb-heroku hosted on heroku

### sbt-scalaxb

To call scalaxb from sbt 0.13.x, put this in your `project/scalaxb.sbt`:

    resolvers += Resolver.sonatypeRepo("public")
    addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "X.X")

and this in `scalaxb.sbt`:

```scala
lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    name := "foo-project",
    scalaxbPackageName in (Compile, scalaxb) := "generated",
    // scalaxbAutoPackages in (Compile, scalaxb) := true
    scalaxbDispatchVersion in (Compile, scalaxb) := "0.11.3"
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

  [1]: https://github.com/eed3si9n/scalaxb/blob/master/INSTALL.md
  [2]: http://scalaxb.org/mvn-scalaxb
  [3]: http://scalaxb.org/issue-reporting-guideline
