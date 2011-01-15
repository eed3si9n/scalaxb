import sbt._
import java.io.{File, InputStream, FileWriter}

trait ScalaBazaarTask extends ScalaScriptTask with ProguardProject {  
  def ouputLibPath = (outputPath ##) / "lib"
  
  def sbazName = name
  override def scriptName = sbazName
  
  def versionlessJarName = sbazName + ".jar"
  def versionlessJarPath = ouputLibPath / versionlessJarName
  def bazaarPackageName = sbazName + "-" + version + ".sbp"
  def bazaarPackagePath = (outputPath ##) / bazaarPackageName
  def bazaarAdvertName = sbazName + "-" + version + ".advert"
  def bazaarAdvertPath = (outputPath ##) / bazaarAdvertName  
  def outputMetaPath = (outputPath ##) / "meta"
  def descriptionPath = outputMetaPath / "description"
  def outputDocPath = (outputPath ##) / "doc"
  def bazaarDepends: List[String] = Nil
  def description: String
  def bazaarPackageBaseURL: String
    
  lazy val sbazPack = sbazPackTask(bazaarDepends, Some(description))
  
  def sbazPackTask(depends: List[String], description: Option[String]) = task {
    if (!outputMetaPath.asFile.exists)
      outputMetaPath.asFile.mkdir
    
    val pack = <package>
  <name>{sbazName}</name>
  <version>{version}</version>{
if (!depends.isEmpty)
    <depends>{
      for (depend <- depends)
        yield <name>{depend}</name>
    }</depends>
  else
    Nil
}{
  if (!description.isEmpty)
    <description>{description.get}</description>
  else
    Nil
}</package>

    val advert = <availablePackage>
  {pack}
  <link>{bazaarPackageBaseURL + bazaarPackageName}</link>
</availablePackage>

    writeFile(descriptionPath.asFile, pack.toString)
    writeFile(bazaarAdvertPath.asFile, advert.toString)
    
    FileUtilities.zip(List(outputBinPath, ouputLibPath, outputDocPath, outputMetaPath),
      bazaarPackagePath, true, log)  
    None
  }.dependsOn(proguard, doc, scalascript)
  
  lazy val versionlessPackage = versionLessPackageAction
  
  def versionLessPackageAction = packageTask(
    packagePaths,
    versionlessJarPath,
    packageOptions).dependsOn(compile) dependsOn(compile) describedAs("Creates a versionless jar file.")
  
  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
  override def proguardOptions = "-dontoptimize" :: "-dontobfuscate" :: proguardKeepAllScala :: proguardKeepMain("*") :: Nil
  override def minJarName = versionlessJarName
  override def minJarPath = versionlessJarPath
  
  private def writeFile(file: File, content: String) =
    if (file.exists() && !file.canWrite())
      error("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
    }
}
