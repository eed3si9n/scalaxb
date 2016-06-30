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

Initially `sbt compile` may result in errors such as

    [error] (scalaxbPlugin/*:update) sbt.ResolveException: unresolved dependency: org.scalaxb#scalaxb_2.10;1.4.1-SNAPSHOT: not found

Then overcome this by temporarily removing `scalaxbPlugin` from

    lazy val root = (project in file(".")).
    aggregate(app, integration, scalaxbPlugin).

as in

    aggregate(app, integration). //, scalaxbPlugin).


Then do in sbt: `publish-local`.
You may have to do first

    find ~/.ivy2/ -type d -name "*scalaxb*" -print0 | xargs -0 rm -rf
