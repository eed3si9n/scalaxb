import sbt._
import Keys._
import sbtbuildinfo.BuildInfoKey
import sbtbuildinfo.BuildInfoKeys._

object Common {
  val Xsd = config("xsd") extend(Compile)
  val Wsdl = config("wsdl") extend(Compile)
  val Soap11 = config("soap11") extend(Compile)
  val Soap12 = config("soap12") extend(Compile)

  val scalaxbCodegenSettings = Nil
  // val scalaxbCodegenSettings: Seq[Def.Setting[_]] = {
  //   import sbtscalaxb.Plugin._
  //   import ScalaxbKeys._
  //   def customScalaxbSettings(base: String): Seq[Project.Setting[_]] = Seq(
  //     sources <<= xsdSource map { xsd => Seq(xsd / (base + ".xsd")) },
  //     sourceManaged <<= baseDirectory / "src_managed",
  //     packageName := base,
  //     protocolFileName := base + "_xmlprotocol.scala",
  //     classPrefix := Some("X")
  //   )

  //   def soapSettings(base: String): Seq[Project.Setting[_]] = Seq(
  //     sources <<= xsdSource map { xsd => Seq(xsd / (base + ".xsd")) },
  //     sourceManaged <<= sourceDirectory(_ / "main" / "resources"),
  //     packageName := base,
  //     protocolFileName := base + "_xmlprotocol.scala",
  //     packageDir := false,
  //     generate <<= (generate) map { files =>
  //       val renamed = files map { file => new File(file.getParentFile, file.getName + ".template") }
  //       IO.move(files zip renamed)
  //       renamed
  //     }
  //   )

  //   inConfig(Xsd)(baseScalaxbSettings ++ inTask(scalaxb)(customScalaxbSettings("xmlschema"))) ++
  //   inConfig(Wsdl)(baseScalaxbSettings ++ inTask(scalaxb)(customScalaxbSettings("wsdl11"))) ++
  //   inConfig(Soap11)(baseScalaxbSettings ++ inTask(scalaxb)(soapSettings("soapenvelope11"))) ++
  //   inConfig(Soap12)(baseScalaxbSettings ++ inTask(scalaxb)(soapSettings("soapenvelope12")))
  // }

  val codegenSettings: Seq[Def.Setting[_]] = scalaxbCodegenSettings ++ Seq(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "src_managed",
    buildInfoPackage := "scalaxb",
    buildInfoKeys := BuildInfoKey.ofN(name, version, scalaVersion, sbtVersion,
      "defaultDispatchVersion" -> Dependencies.defaultDispatchVersion,
      "defaultGigahorseVersion" -> Dependencies.defaultGigahorseVersion,
      "defaultGigahorseBackend" -> Dependencies.defaultGigahorseBackend),
  )

  val sonatypeSettings: Seq[Def.Setting[_]] = Seq(
    publishArtifact in Test := false,
    resolvers ++= Seq(
      "sonatype-public" at "https://oss.sonatype.org/content/repositories/public"),
    publishTo := {
      val v = version.value
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }
  )
}
