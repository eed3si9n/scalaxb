import sbt._
import Keys._

object Dependencies {
  val scala211 = "2.11.7"
  val scala210 = "2.10.4"

  val scopt = "com.github.scopt" %% "scopt" % "3.2.0"
  val log4j = "log4j" % "log4j" % "1.2.17"
  val defaultDispatchVersion = "0.11.1"
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % defaultDispatchVersion
  val launcherInterface = "org.scala-sbt" % "launcher-interface" % "0.12.0"
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
  val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  val cxfVersion = "3.0.2"
  val cxfFrontendJaxws = "org.apache.cxf" % "cxf-rt-frontend-jaxws" % cxfVersion
  val cxfTransportsHttp = "org.apache.cxf" % "cxf-rt-transports-http" % cxfVersion
  val cxfTrapsportsHttpJetty = "org.apache.cxf" % "cxf-rt-transports-http-jetty" % cxfVersion
  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.6"

  def scalaCompiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  val specs2Version = "2.4.11"
  def specs2(sv: String) = sv match {
    case x if x startsWith "2.9."  => "org.specs2" %% "specs2" % "1.12.3"
    case x if x startsWith "2.10." => "org.specs2" %% "specs2" % "2.1.1"
    case _ => "org.specs2" % "specs2_2.11" % "2.3.13"
  }

  def appDependencies(sv: String) = Seq(
    launcherInterface % "provided",
    scopt,
    log4j
  ) ++ (sv match {
    case x if sv startsWith "2.10." => Nil
    case _ => Seq(scalaXml, scalaParser)
  })
  def integrationDependencies(sv: String) = Seq(
    dispatch % "test",
    scalaCompiler(sv),
    specs2(sv) % "test",
    cxfFrontendJaxws % "test",
    cxfTransportsHttp % "test",
    cxfTrapsportsHttpJetty % "test",
    scalaz % "test"
  )
}
