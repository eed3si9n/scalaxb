scalaxb
=======

scalaxb is an XML data-binding tool for Scala that supports W3C XML 
Schema (xsd) as the input file.

Status
------

Some things may not work.
I'd really appreciate if you could run it against your favorite xsd
file and let me know the result.

sbt-scalaxb for sbt 0.10.1
--------------------------

To call scalaxb from sbt 0.10.1, put this in your `project/plugins/build.sbt`:

    libraryDependencies <+= (sbtVersion) { sv => "org.scalaxb" %% "sbt-scalaxb" % ("sbt" + sv + "_0.6.2") }

and this in `build.sbt`:

    seq(sbtscalaxb.Plugin.scalaxbSettings: _*)

    sourceGenerators in Compile <+= scalaxb.identity

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
      --prepend-family
            prepends family name to class names
      --wrap-contents <complexType>
            wraps inner contents into a seperate case class
      --contents-limit <size>
            defines long contents to be segmented (default: 20)
      --chunk-size <size>
            segments long sequences into chunks (default: 10)
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
