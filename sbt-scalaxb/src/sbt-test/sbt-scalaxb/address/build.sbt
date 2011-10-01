import ScalaxbKeys._

name := "mavenxsd"

seq(scalaxbSettings: _*)

packageName in scalaxb in Compile := "ipo"

sourceGenerators in Compile <+= scalaxb in Compile
