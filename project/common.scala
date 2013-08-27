import sbt._
import Keys._
import sbtbuildinfo.Plugin._
import sbtscalashim.Plugin._

object Common {
  val Xsd = config("xsd") extend(Compile)
  val Wsdl = config("wsdl") extend(Compile)
  val Soap11 = config("soap11") extend(Compile)
  val Soap12 = config("soap12") extend(Compile)

  // val scalaxbCodegenSettings = Nil
  val scalaxbCodegenSettings: Seq[Def.Setting[_]] = {
    import sbtscalaxb.Plugin._
    import ScalaxbKeys._
    def customScalaxbSettings(base: String): Seq[Project.Setting[_]] = Seq(
      sources <<= xsdSource map { xsd => Seq(xsd / (base + ".xsd")) },
      sourceManaged <<= baseDirectory / "src_managed",
      packageName := base,
      protocolFileName := base + "_xmlprotocol.scala",
      classPrefix := Some("X")
    )

    def soapSettings(base: String): Seq[Project.Setting[_]] = Seq(
      sources <<= xsdSource map { xsd => Seq(xsd / (base + ".xsd")) },
      sourceManaged <<= sourceDirectory(_ / "main" / "resources"),
      packageName := base,
      protocolFileName := base + "_xmlprotocol.scala",
      packageDir := false,
      generate <<= (generate) map { files =>
        val renamed = files map { file => new File(file.getParentFile, file.getName + ".template") }
        IO.move(files zip renamed)
        renamed
      }
    )

    inConfig(Xsd)(baseScalaxbSettings ++ inTask(scalaxb)(customScalaxbSettings("xmlschema"))) ++
    inConfig(Wsdl)(baseScalaxbSettings ++ inTask(scalaxb)(customScalaxbSettings("wsdl11"))) ++
    inConfig(Soap11)(baseScalaxbSettings ++ inTask(scalaxb)(soapSettings("soapenvelope11"))) ++
    inConfig(Soap12)(baseScalaxbSettings ++ inTask(scalaxb)(soapSettings("soapenvelope12")))
  }

  val codegenSettings: Seq[Def.Setting[_]] = buildInfoSettings ++ scalaShimSettings ++ scalaxbCodegenSettings ++ Seq(
    unmanagedSourceDirectories in Compile <+= baseDirectory( _ / "src_managed" ),
    buildInfoPackage := "scalaxb",
    sourceGenerators in Compile <+= buildInfo,
    sourceGenerators in Compile <+= scalaShim 
  )

  // val customLsSettings: Seq[Def.Setting[_]] = Nil
  val customLsSettings: Seq[Def.Setting[_]] = {
    import ls.Plugin.{LsKeys => lskeys}
    _root_.ls.Plugin.lsSettings ++ Seq(
      lskeys.tags in lskeys.lsync := Seq("xml", "soap", "wsdl", "code-generation"),
      (externalResolvers in lskeys.lsync) := Seq(Resolver.sonatypeRepo("public"))
    )
  }

  val sonatypeSettings: Seq[Def.Setting[_]] = Seq(
    pomExtra := (<scm>
        <url>git@github.com:eed3si9n/scalaxb.git</url>
        <connection>scm:git:git@github.com:eed3si9n/scalaxb.git</connection>
      </scm>
      <developers>
        <developer>
          <id>eed3si9n</id>
          <name>Eugene Yokota</name>
          <url>http://eed3si9n.com</url>
        </developer>
      </developers>),
    publishArtifact in Test := false,
    resolvers ++= Seq(
      "sonatype-public" at "https://oss.sonatype.org/content/repositories/public"),
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots") 
      else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishMavenStyle := true,
    pomIncludeRepository := { x => false }
  )
}
