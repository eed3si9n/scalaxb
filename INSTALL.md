scalaxb installation
====================

scalaxb is tested under Scala 2.9.0-1, but should also work for 2.8.1.
It also requires Java SE 6.

Use conscript
=============

Install conscript (cs).

- https://github.com/n8han/conscript/

Install scalaxb using it.

    $ cs eed3si9n/scalaxb

Install a SNAPSHOT version of scalaxb.

    $ cs eed3si9n/scalaxb/x.x.x-SNAPSHOT


Notes for conscript on Mac
==========================

Make sure `wget` has been installed

    brew install wget

Download and run install script

    curl https://raw.githubusercontent.com/foundweekends/conscript/master/setup.sh >setup-conscript.sh
    chmod u+x setup-conscript.sh
    ./setup-conscript.sh


Build from source
=================================

If you want to build from source, install git.

- http://git-scm.com/

Download and install simple-build-tool (sbt 0.10).

- https://github.com/harrah/xsbt/wiki/Setup
  
Grab scalaxb's source and build:
   
    $ git clone git://github.com/eed3si9n/scalaxb.git scalaxb
    $ cd scalaxb

Open `project/build.scala` using your favorite editor, and change the version number to `local-SNAPSHOT`.
This way our Ivy cache won't be confused when the official builds are released.

    version := "local-SNAPSHOT",

Build, and publish it locally.

    $ sbt "project app" "+ publish-local"

Finally, install it using conscript.

    $ cs --local eed3si9n/scalaxb/local-SNAPSHOT


Don't use sbaz
==============

NOTE: If you have previously installed scalaxb using sbaz, remove it
because it interferes with your code:

    $ sudo sbaz remove scalaxb

TBD
===

For local development&testing, change version number to 1.4.1-SNAPSHOT

In build.sbt:
    version in ThisBuild := "1.4.1-SNAPSHOT",
    ...
    crossScalaVersions := Seq(scala210),
    scalaVersion := scala210,

In pom.xml:
  <version>1.4.1-SNAPSHOT</version>

Initially `sbt compile` may result in

    [error] (scalaxbPlugin/*:update) sbt.ResolveException: unresolved dependency: org.scalaxb#scalaxb_2.10;1.4.1-SNAPSHOT: not found

Then overcome this by temporarily removing `scalaxbPlugin` from

    lazy val root = (project in file(".")).
    aggregate(app, integration, scalaxbPlugin).

as in

    aggregate(app, integration). //, scalaxbPlugin).


Then do in sbt: `publish-local`.
You may have to do first
    find ~/.ivy2/ -type d -name "*scalaxb*" -print0 | xargs -0 rm -rf

Note: during `publish-local` it is possible to get errors such as
    ...
    [info] 	published ivy to /Users/andrevandelft/.ivy2/local/org.scalaxb/scalaxb_2.11/1.4.1-SNAPSHOT/ivys/ivy.xml
    [error] /Users/andrevandelft/Documents/workspaces/scalaxb/sbt-scalaxb/src/main/scala/sbtscalaxb/Plugin.scala:90: can't expand macros compiled by previous versions of Scala
    [error]     logLevel in scalaxb <<= logLevel?? Level.Info
    [error]                                              ^

Then make sure in build.sbt all scala versions are 210:
    scalaVersion := scala210,

Run conscript to get a proper `scalaxb`. Make sure ~/.conscript/bin is in the PATH; then:
    cs eed3si9n/scalaxb

Check
    /Users/andrevandelft/.conscript/eed3si9n/scalaxb/scalaxb/launchconfig
