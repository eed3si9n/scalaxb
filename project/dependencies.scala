import sbt._
import Keys._

object Dependencies {
  val scopt = "com.github.scopt" %% "scopt" % "3.0.0"
  val log4j = "log4j" % "log4j" % "1.2.17"
  val defaultDispatchVersion = "0.11.1"
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % defaultDispatchVersion
  val launcherInterface = "org.scala-sbt" % "launcher-interface" % "0.12.0"

  def scalaCompiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  val specs2Version = "2.1.1"
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.9." => "org.specs2" %% "specs2" % "1.12.3"
    case _ => "org.specs2" %% "specs2" % specs2Version
  }

  val appDependencies = Seq(
    launcherInterface % "provided",
    scopt,
    log4j
  )
  def integrationDependencies(sv: String) = Seq(
    dispatch % "test",
    scalaCompiler(sv),
    specs2(sv) % "test"
  )
}
