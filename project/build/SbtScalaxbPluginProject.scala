import sbt._

class SbtScalaxbPluginProject(info: ProjectInfo) extends PluginProject(info) {
  val scalaxb = "org.scalaxb" % "scalaxb_2.8.1" % "0.5.4-SNAPSHOT"
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.sftp("repobum", "repobum", "/home/public/%s".format(
    if (projectVersion.value.toString.endsWith("-SNAPSHOT")) "snapshots"
    else "releases"
  )) as("repobum_repobum", new java.io.File(Path.userHome + "/.ssh/repobum"))
}
