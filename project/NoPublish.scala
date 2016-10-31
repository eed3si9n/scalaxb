import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object NoPublish extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def projectSettings = Seq(
    publishArtifact := false,
    publish := (),
    publishLocal := (),
    publishSigned := ()
  )
}
