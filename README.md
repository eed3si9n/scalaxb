scalaxb
=======

scalaxb is an XML data-binding tool for Scala that supports W3C XML 
Schema (xsd) as the input file.

Status
------

Some things may not work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

sbt-scalaxb for sbt 0.10
------------------------

To call `scalaxb` from sbt 0.10, put this in your `project/plugins/build.sbt`:

    libraryDependencies += "org.scalaxb" %% "sbt-scalaxb" % "0.6.1"

    sourceGenerators in Compile <+= scalaxb.identity

sbt-scalaxb
-----------

To call `compile-xsd` from sbt, put this in your Plugins.scala:

    import sbt._

    class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
      val scalaxb = "org.scalaxb" % "sbt-scalaxb" % "0.6.1"
      
      val scalaToolsNexusSnapshots = "Scala Tools Nexus Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
      val scalaToolsNexusReleases  = "Scala Tools Nexus Releases" at "http://nexus.scala-tools.org/content/repositories/releases/"
    }

`scalaxb` command line
----------------------

See [INSTALL.md][1].

Usage
-----

    $ scalaxb [options] <schema_file>...

      -d <directory> | --outdir <directory>
            generated files will go into <directory>
      -p <package> | --package <package>
            specifies the target package
      -p:<namespaceURI>=<package> | --package:<namespaceURI>=<package>
            specifies the target package for <namespaceURI>
      --class-prefix <prefix>
            prefixes generated class names
      --param-prefix <prefix>
            prefixes generated parameter names
      --wrap-contents <complexType>
            wraps inner contents into a seperate case class
      --chunk-size <size>
            segments long sequnces into chunks of <size>
      --package-dir
            generates package directories
      --no-runtime
            skips runtime files
      -v | --verbose
            be extra verbose
      <schema_file>
            input schema to be converted

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
