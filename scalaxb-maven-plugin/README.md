scalaxb-maven-plugin
====================

This is the maven plugin for invoking scalaxb as part of a maven
build. By default, it binds to the generate-sources phase of the
build lifecycle, allowing Scala sources to be generated from XSD
files, and subsequently included in the rest of the build.

Building
--------

To build with integration tests:

    mvn install -s settings.xml -Pit

To build without integration tests, simply:

    mvn install -s settings.xml

The `-s` option can be omitted when using a repository manager,
or if your existing user settings already specify the scala-tools
repository.

Examples
--------

The easiest way to see an example project that uses the scalaxb
maven plugin is to run the integration tests (as above, or using
`mvn verify -Pit`) and see integration test projects that have
been built in subdirectories of `target/it/`.
