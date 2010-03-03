import sbt._
import java.io.{File, InputStream, FileWriter}

trait ScalaBazaarTask extends DefaultProject {  
  def ouputLibPath = (outputPath ##) / "lib"
  override val defaultJarName = name + ".jar"
  override def jarPath = ouputLibPath / defaultJarName
  def outputBinPath = (outputPath ##) / "bin"
  val bazaarPackageName = name + "-" + version + ".sbp"
  def bazaarPackagePath = outputPath / bazaarPackageName
  val bazaarAdvertName = name + "-" + version + ".advert"
  def bazaarAdvertPath = outputPath / bazaarAdvertName  
  def outputMetaPath = (outputPath ##) / "meta"
  def descriptionPath = outputMetaPath / "description"
  def outputDocPath = (outputPath ##) / "doc"
  
  def sbazTask(depends: List[String], description: Option[String]) = task {
    if (!outputMetaPath.asFile.exists)
      outputMetaPath.asFile.mkdir
    
    val pack = <package>
  <name>{name}</name>
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
  <link>INSERT LINK HERE</link>
</availablePackage>

    writeFile(descriptionPath.asFile, pack.toString)
    writeFile(bazaarAdvertPath.asFile, advert.toString)
    
    FileUtilities.zip(List(outputBinPath, jarPath, outputDocPath, outputMetaPath),
      bazaarPackagePath, true, log)  
    None
  }
  
  private def writeFile(file: File, content: String) =
    if (file.exists() && !file.canWrite())
      error("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      writer.write(content)
      writer.close()
    }
}
