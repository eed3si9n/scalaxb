scalaxb installation
====================

scalaxb is tested under Scala 2.8.1, but should also work for 2.9.0-1.
It also requires Java SE 6.

Don't use sbaz
==============

NOTE: If you have previously installed scalaxb using sbaz, remove it
because it interferes with your code:

    $ sudo sbaz remove scalaxb

Use conscript
=============

Install conscript (cs).

- https://github.com/n8han/conscript/

Install scalaxb using it.

    $ cs eed3si9n/scalaxb

Build from source
=================================

If you want to build from source, install git.

- http://git-scm.com/

Download and install simple-build-tool (sbt 0.7).

- http://code.google.com/p/simple-build-tool/wiki/Build
  
Grab scalaxb's source and build:
   
    $ git clone git://github.com/eed3si9n/scalaxb.git scalaxb
    $ cd scalaxb
    $ sbt update "project scalaxb" update proguard

This downloads the dependent libraries and produces
- target/scala_2.8.1/scalaxb-x.x.x.min.jar

Manually install the jar file.

Unix
----

Copy the jar in your ~/bin directory, then put the line

    java -Xmx1G -cp "scalaxb-x.x.x.min.jar" scalaxb.compiler.Main "$@"

in a file called scalaxb in your ~/bin directory and do

    $ chmod u+x ~/bin/scalaxb
  
This allows you to launch scalaxb in any directory by typing `scalaxb` at the
terminal.

Windows
-------

Create a batch file scalaxb.bat:

    set SCRIPT_DIR=%~dp0
    java -Xmx1G -jar "%SCRIPT_DIR%scalaxb-x.x.x.min.jar" %*

and put the jar in the same directory as the batch file. Put scalaxb.bat on your
path so that you can launch scalaxb in any directory by typing `scalaxb` at the
command prompt.
