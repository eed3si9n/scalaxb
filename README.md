scalaxb
=======

scalaxb is an XML data-binding tool for Scala that supports W3C XML 
Schema (xsd) as the input file.

Status
------

The latest is 0.6.5. Some things may not work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

sbt-scalaxb for sbt 0.11.0
--------------------------

To call scalaxb from sbt 0.11.0, put this in your `project/plugins.sbt`:

    libraryDependencies <+= (sbtVersion) { sv => "org.scalaxb" %% "sbt-scalaxb" % ("sbt" + sv + "_X.X") }

and this in `build.sbt`:

    seq(scalaxbSettings: _*)

    packageName in scalaxb in Compile := "xxx"

    sourceGenerators in Compile <+= scalaxb in Compile

`scalaxb` command line
----------------------

See [INSTALL.md][1].

Documents
---------

Further info is available at [scalaxb.org](http://scalaxb.org/).

Bug Reporting
-------------

You can send bug reports to [Issues](http://github.com/eed3si9n/scalaxb/issues),
send me a [tweet to @scalaxb](http://twitter.com/scalaxb), or email.

Licensing
---------

It's the MIT License. See the file called LICENSE.
     
Contacts
--------

- [mailing list](http://groups.google.com/group/scalaxb)
- [@scalaxb](http://twitter.com/scalaxb)

  [1]: https://github.com/eed3si9n/scalaxb/blob/master/INSTALL.md
