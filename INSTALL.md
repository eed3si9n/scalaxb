scalaxb installation
====================

scalaxb is tested under Scala 2.11.

Use coursier
============

Install coursier (cs).
- https://get-coursier.io/docs/cli-installation

Install scalaxb using it.

    $ cs install --contrib scalaxb

Use conscript
=============

Install conscript (cs).

- https://github.com/foundweekends/conscript

Install scalaxb using it.

    $ cs eed3si9n/scalaxb

Install a SNAPSHOT version of scalaxb.

    $ cs eed3si9n/scalaxb/x.x.x-SNAPSHOT

Build from source
=================================

If you want to build from source, install git.

- http://git-scm.com/

Download and install sbt.

- https://www.scala-sbt.org/download.html

Grab scalaxb's source and build:

    $ git clone git@github.com:eed3si9n/scalaxb.git scalaxb
    $ cd scalaxb

Open `build.sbt` using your favorite editor, and change the version number to `local-SNAPSHOT`.
This way our Ivy cache won't be confused when the official builds are released.

    version := "local-SNAPSHOT",

Build, and publish it locally.

    $ sbt "project app" "publishLocal"

Finally, install it using conscript.

    $ cs --local eed3si9n/scalaxb/local-SNAPSHOT
