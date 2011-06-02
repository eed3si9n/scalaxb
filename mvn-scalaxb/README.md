scalaxb Maven Plugin
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

Releasing
---------

From the `mvn-scalaxb` directory:

  * Copy the settings file to somewhere outside the repository, and
    edit it to set the username and password used to deploy to the
    scala-tools repository:

        cp settings.xml ~/.m2/scala-tools.xml
        vi ~/.m2/scala-tools.xml

  * Create an alias that will invoke Maven using the new settings file:

        alias mvnst="mvn -s ~/.m2/scala-tools.xml"

  * Use the `maven-release-plugin` to update the version in the `pom.xml`,
    tag, and verify the build:

        mvnst release:prepare

  * Use the release plugin again to checkout the tag, build the
    release, and deploy to scala-tools.org

        mvnst release:perform

The above process will leave a tag in the local repository called
'scalaxb-maven-plugin-*version*'. It won't be pushed to github
automatically.  Since scalaxb releases are already tagged, there
doesn't seem to be any benefit to having the tag created by the
release plugin in the github repo. It can be deleted with `git tag
-d`.

Examples
--------

The easiest way to see an example project that uses the scalaxb
maven plugin is to run the integration tests (as above, or using
`mvn verify -Pit`) and see integration test projects that have
been built in subdirectories of `target/it/`.
