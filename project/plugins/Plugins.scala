import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
  val posterous = "net.databinder" % "posterous-sbt" % "0.1.4"
  val proguard = "org.scala-tools.sbt" % "sbt-proguard-plugin" % "0.0.+"
  val appenginePlugin = "net.stbbs.yasushi" % "sbt-appengine-plugin" % 
    "2.1-SNAPSHOT" from "http://cloud.github.com/downloads/Yasushi/sbt-appengine-plugin/sbt-appengine-plugin-2.1.jar"
}
