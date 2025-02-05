import sbt._

object Dependencies {
  val scala3   = "3.3.1"
  val scala213 = "2.13.12"
  val scala212 = "2.12.18"

  val jaxb = "javax.xml.bind" % "jaxb-api" % "2.3.1"
  def scopt(sv: String) = {
    CrossVersion.partialVersion(sv) match {
      case Some((2, _)) =>
        "com.github.scopt" %% "scopt" % "3.7.1"
      case _ =>
        "com.github.scopt" %% "scopt" % "4.1.0"
    }
  }
  val log4j = "log4j" % "log4j" % "1.2.17"
  val defaultDispatchVersion = "1.0.1"
  def dispatch(sv: String) = CrossVersion partialVersion sv match {
    case Some((2, x)) if x >= 13 => "org.dispatchhttp" %% "dispatch-core" % "1.1.0"
    case Some(_)                 => "org.dispatchhttp" %% "dispatch-core" % "1.0.1"
    case x                       => sys error s"Unexpected Scala version [$sv], with partial version $x"
  }
  val defaultHttp4sVersion = "0.23.18"
  val http4s =  "org.http4s" %% "http4s-client" % defaultHttp4sVersion
  val http4sEmber =  "org.http4s" %% "http4s-ember-client" % defaultHttp4sVersion
  val defaultGigahorseVersion = "0.5.0"
  val defaultGigahorseBackend = "okhttp"
  val gigahorse = "com.eed3si9n" %% s"gigahorse-$defaultGigahorseBackend" % defaultGigahorseVersion
  val launcherInterface = "org.scala-sbt" % "launcher-interface" % "0.12.0"
  val scalaXml2 = "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
  val scalaParserCombinators1 = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  val scalaParserCombinators2 = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
  val cxfVersion = "3.3.1"
  val cxfFrontendJaxws = "org.apache.cxf" % "cxf-rt-frontend-jaxws" % cxfVersion
  val cxfFrontendJaxrs = "org.apache.cxf" % "cxf-rt-frontend-jaxrs" % cxfVersion
  val cxfTransportsHttp = "org.apache.cxf" % "cxf-rt-transports-http" % cxfVersion
  val cxfTrapsportsHttpJetty = "org.apache.cxf" % "cxf-rt-transports-http-jetty" % cxfVersion
  def monocleCore(sv: String) = {
    CrossVersion.partialVersion(sv) match {
      case Some((2, v)) if v <= 12 =>
        "com.github.julien-truffaut" %% "monocle-core" % "2.0.3"
      case _ =>
        "dev.optics" %% "monocle-core" % "3.2.0"
    }
  }
  def monocleMacro(sv: String) = {
    CrossVersion.partialVersion(sv) match {
      case Some((2, v)) if v <= 12 =>
        "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3"
      case _ =>
        "dev.optics" %% "monocle-macro" % "3.2.0"
    }
  }

  def scalaCompiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  def specs2(sv: String) = CrossVersion partialVersion sv match {
    case Some((2, 12)) => "org.specs2" %% "specs2" % "2.4.17"
    case Some((2, 13)) => "org.specs2" %% "specs2-matcher-extra" % "4.6.0"
    case x             => sys error s"Unexpected Scala version [$sv], with partial version $x"
  }

  def appDependencies(sv: String) = Seq(
    launcherInterface % "provided",
    jaxb % "provided",
    scopt(sv),
    log4j
  ) ++ (sv match {
    case x if sv.startsWith("2.12.") => Seq(scalaXml2, scalaParserCombinators1)
    case x                           => Seq(scalaXml2, scalaParserCombinators2)
  })
  def integrationDependencies(sv: String) = Seq(
    dispatch(sv) % "test",
    http4s % "test",
    http4sEmber % "test",
    gigahorse % "test",
    scalaCompiler(sv),
    specs2(sv) % "test",
    cxfFrontendJaxws % "test",
    cxfFrontendJaxrs % "test",
    cxfTransportsHttp % "test",
    cxfTrapsportsHttpJetty % "test",
    monocleCore(sv) % Test,
    monocleMacro(sv) % Test,
  )
}
