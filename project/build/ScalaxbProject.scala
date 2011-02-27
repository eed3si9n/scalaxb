import sbt._

class ScalaxbProject(info: ProjectInfo) extends ParentProject(info) {
  lazy val parentPath = path(".")
  
  val scalaToolsSnapshots      = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsNexusSnapshots = "Scala Tools Nexus Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  val scalaToolsNexusReleases  = "Scala Tools Nexus Releases" at "http://nexus.scala-tools.org/content/repositories/releases/"
   
  lazy val cli = project("cli", "scalaxb", new CliProject(_))
  
  class CliProject(info: ProjectInfo) extends DefaultProject(info) with VersionFileTask
      with ScalaBazaarTask with posterous.Publish {
    val scopt = "com.github.scopt" %% "scopt" % "1.0.0-SNAPSHOT"    
            
    override def description = "XML data binding tool for Scala."
    override def bazaarPackageBaseURL = "http://cloud.github.com/downloads/eed3si9n/scalaxb/"
    override def notesPath = parentPath / "notes"
    override def versionFilePackage = "scalaxb"
    override def compileAction = super.compileAction dependsOn(versionfile)
  }
  
  lazy val web = project("web", "scalaxb-appengine", new WebProject(_), cli)
  class WebProject(info: ProjectInfo) extends AppengineProject(info) with GenerateClientTask {
    override def localizationsPath = parentPath / "project" / "build" / "localizations"
    
    val uf_version = "0.2.2"
    lazy val uf         = "net.databinder" %% "unfiltered-filter" % uf_version
    lazy val uf_uploads = "net.databinder" %% "unfiltered-uploads" % uf_version
    // lazy val uf_spec    = "net.databinder" %% "unfiltered-spec" % uf_version

    lazy val deployLocal = deployLocalTask

    def deployLocalTask = task {
      args => 
        devAppserverStartTask(args) dependsOn(prepareWebapp, devAppserverStop, devAppserverStop) 
    }
  }
  
  lazy val integration = project("integration", "scalaxb integration", new IntegrationProject(_), cli)
  
  class IntegrationProject(info: ProjectInfo) extends DefaultProject(info) {
    val specsVersion = crossScalaVersionString match {
      case "2.8.0" => "1.6.5"
      case _ => "1.6.6"
    }
    val specs = "org.scala-tools.testing" % ("specs_" + crossScalaVersionString) % specsVersion % "test"
    val junit = "junit" % "junit" % "4.7" % "test"
    
    override def testCompileOptions = super.testCompileOptions ++ Seq(CompileOption("-no-specialization"))
  }  
  
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  // val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
