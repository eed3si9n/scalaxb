scalaxb Maven Plugin
====================

This is the maven plugin for invoking scalaxb as part of a maven
build. By default, it binds to the generate-sources phase of the
build lifecycle, allowing Scala sources to be generated from XSD
files, and subsequently included in the rest of the build.

User documentation
------------------

 * [Getting Started](http://martiell.github.com/scalaxb/maven/usage.html)
 * [Configuration Reference](http://martiell.github.com/scalaxb/maven/generate-mojo.html)
 * [scalaxb.org/mvn-scalaxb](http://scalaxb.org/mvn-scalaxb)

Building
--------

To build with integration tests:

    mvn install -Pit

To build without integration tests, simply:

    mvn install

Publishing
----------

To deploy to Sonatype OSS repository hosting, run the publish
script.

* Check the version in the pom.xml.
* Run `./publish'.
* Enter credentials for Sonatype Nexus instance.
* Update the version in the pom.xml to the next snapshot version
  after publishing a release.

Further documentation is included at the top of the `publish`
script.

Examples
--------

The easiest way to see an example project that uses the scalaxb
maven plugin is to run the integration tests (as above, or using
`mvn verify -Pit`) and see integration test projects that have
been built in subdirectories of `target/it/`.
