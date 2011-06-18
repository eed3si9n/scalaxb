import sbt._

class ScalaxbProject(info: ProjectInfo) extends ParentProject(info) {
  lazy val parentPath = path(".")
  
  val scalaToolsSnapshots      = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsNexusSnapshots = "Scala Tools Nexus Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  val scalaToolsNexusReleases  = "Scala Tools Nexus Releases" at "http://nexus.scala-tools.org/content/repositories/releases/"
   
  lazy val cli = project("cli", "scalaxb", new CliProject(_))
  
  class CliProject(info: ProjectInfo) extends DefaultProject(info) with VersionFileTask
      with ProguardProject with posterous.Publish with TestProject with scalaxb.ScalaxbPlugin {
    val scopt = "com.github.scopt" %% "scopt" % "1.1.1"
    val launch = "org.scala-tools.sbt" % "launcher-interface" % "0.7.4" % "provided" from (
      "http://databinder.net/repo/org.scala-tools.sbt/launcher-interface/0.7.4/jars/launcher-interface.jar")

    override def notesPath = parentPath / "notes"
    override def versionFilePackage = "scalaxb"
    override def generatedPackageName = "wsdl20"
    override def generatedClassPrefix = Some("X")
    override def compileAction = super.compileAction dependsOn(versionfile)
    override def minJarName = "scalaxb-%s.min.jar" format version
    override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
    override def proguardOptions = List("-dontoptimize",
      "-dontobfuscate",
      "-dontnote",
      "-dontwarn",
      "-ignorewarnings",
      proguardKeepAllScala,
      proguardKeepMain("*"))
  }
  
  lazy val web = project("web", "scalaxb-appengine", new WebProject(_), cli)
  class WebProject(info: ProjectInfo) extends AppengineProject(info) with GenerateClientTask with NoPublish {
    override def localizationsPath = parentPath / "project" / "build" / "localizations"
    
    val uf_version = "0.3.3"
    lazy val uf         = "net.databinder" %% "unfiltered-filter" % uf_version
    lazy val uf_uploads = "net.databinder" %% "unfiltered-uploads" % uf_version

    lazy val deployLocal = deployLocalTask

    def deployLocalTask = task {
      args => 
        devAppserverStartTask(args) dependsOn(prepareWebapp, devAppserverStop, devAppserverStop) 
    }
  }
  
  lazy val integration = project("integration", "scalaxb-integration", new IntegrationProject(_), cli)
  
  class IntegrationProject(info: ProjectInfo) extends DefaultProject(info) with TestProject with NoPublish
  
  trait TestProject extends DefaultProject {
    override def testCompileOptions = super.testCompileOptions ++ Seq(CompileOption("-no-specialization")) 
    val crossVersionSpecs = crossScalaVersionString match {
      case "2.9.0-1" => "specs_2.8.1"
      case _ => "specs_" + crossScalaVersionString
    }
    val specsVersion = crossScalaVersionString match {
      case "2.8.0" => "1.6.5"
      case _ => "1.6.6"
    }
    val specs = "org.scala-tools.testing" % crossVersionSpecs % specsVersion % "test"
    val junit = "junit" % "junit" % "4.7" % "test"
    val dispatch_http = "net.databinder" %% "dispatch-http" % "0.8.1" % "test"
  }

  trait NoPublish extends BasicManagedProject {
    override protected def publishAction = task {None}
    override protected def publishLocalAction = task {None}
  }
  
  lazy val pluginProject = project("sbt-scalaxb", "sbt-scalaxb", new ScalaxbPluginProject(_))
  
  class ScalaxbPluginProject(info: ProjectInfo) extends PluginProject(info) {
    val scalaxb = "org.scalaxb" % "scalaxb_2.9.0-1" % projectVersion.value.toString
    override def managedStyle = ManagedStyle.Maven
  }

  override def publishAction = task {None}
  override def publishLocalAction = task {None}

  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  // val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
