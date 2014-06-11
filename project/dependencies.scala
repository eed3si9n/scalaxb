import sbt._
import Keys._

object Dependencies {
  val scopt = "com.github.scopt" %% "scopt" % "3.2.0"
  val log4j = "log4j" % "log4j" % "1.2.17"
  val defaultDispatchVersion = "0.11.1"
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % defaultDispatchVersion
  val launcherInterface = "org.scala-sbt" % "launcher-interface" % "0.12.0"
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
  val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

  def scalaCompiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  val specs2Version = "2.3.12"
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.9."  => "org.specs2" %% "specs2" % "1.12.3"
    case x if x startsWith "2.10." => "org.specs2" %% "specs2" % "2.1.1"
    case _ => "org.specs2" %% "specs2" % specs2Version
  }

  val appDependencies = Seq(
    launcherInterface % "provided",
    scalaXml,
    scalaParser,
    scopt,
    log4j
  )
  def integrationDependencies(sv: String) = Seq(
    dispatch % "test",
    scalaCompiler(sv),
    specs2(sv) % "test"
  )
}
