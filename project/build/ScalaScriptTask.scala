import sbt._
import java.io.{File, InputStream, FileWriter}

trait ScalaScriptTask extends DefaultProject {  
  def templatePath = path("project") / "build" / "templates"
  
  def outputBinPath = (outputPath ##) / "bin"
  def scriptPath = outputBinPath / name
  def scriptProperties: List[Tuple2[String, String]] = Nil
  def scriptJavaFlags = "-Xmx256M -Xms16M"
  def scriptToolFlags = ""
  
  lazy val scalascript = scalascriptTask(scriptPath.asFile,
    scriptProperties, scriptJavaFlags, scriptToolFlags)
  
  /** generates shell script to execute the program.
   * uses <code>mainClassValue</code> and <code>runClasspath</code>.
   */
  def scalascriptTask(file: File,
      properties: List[Tuple2[String, String]],
      javaFlags: String,
      toolFlags: String) = task {        
    
    val props = properties.map({
      case Pair(name,value) => "-D" + name + "=\"" + value + "\""
    }).mkString("", " ", "")
    
    val mainClassValue = getMainClass(true) match {
      case Some(x) => x
      case None => error("mainClass was not found.")
    }
    
    val classpath = runClasspath.getPaths
    
    val patches = Map (
      "class" -> mainClassValue,
      "properties" -> props,
      "javaflags" -> javaFlags,
      "toolflags" -> toolFlags
    )
    
    val parentDir = file.getParentFile()
    if (parentDir != null)
      parentDir.mkdir
    
    val platforms = List("unix", "windows")
    // Consolidate Paths into classpath
    // classpath = classpath ::: classpathPath.list.toList
    // Generate the scripts
              
    if (platforms.contains("unix")) {
      val unixclasspath =
        transposeVariableMarkup(classpath.mkString("", ":", "").replace('\\', '/'), "${", "}")
      val unixPatches = patches + ("classpath" -> unixclasspath)
      val unixTemplateResource = templatePath / "tool-unix.tmpl"
      val unixTemplate = readAndPatchResource(unixTemplateResource, unixPatches)
      writeFile(file, unixTemplate)
    }
    
    if (platforms.contains("windows")) {
      val winclasspath =
        transposeVariableMarkup(classpath.mkString("", ";", "").replace('/', '\\'), "%", "%")
      val winPatches = patches + ("classpath" -> winclasspath)
      val winTemplateResource = templatePath / "tool-windows.tmpl"
      val winTemplate = readAndPatchResource(winTemplateResource, winPatches)
      writeFile(new File(file.getAbsolutePath() + ".bat"), winTemplate)
    }
    None
  }
  
  // Converts a variable like @SCALA_HOME@ to ${SCALA_HOME} when pre = "${" and post = "}"
  private def transposeVariableMarkup(text: String, pre: String, post: String) : String = {
    val chars = scala.io.Source.fromString(text)
    val builder = new StringBuilder()
    
    while (chars.hasNext) {
      val char = chars.next
      if (char == '@') {
        var char = chars.next
        val token = new StringBuilder()
        while (chars.hasNext && char != '@') {
          token.append(char)
          char = chars.next
        }
        if (token.toString == "")
          builder.append('@')
        else
          builder.append(pre + token.toString + post)
      } else builder.append(char)          
    }
    builder.toString
  }
  
  private def readAndPatchResource(resource: Path, tokens: Map[String, String]): String = {
    val in = new java.io.FileReader(resource.asFile)
    val builder = new StringBuilder()
    
    while (in.ready) {
      val char: Char = in.read.toChar
      if (char == '@') {
        var char: Char = in.read.toChar
        val token = new StringBuilder()
        
        while (in.ready && char != '@') {
          token.append(char)
          char = in.read.toChar
        }
        
        if (tokens.contains(token.toString))
          builder.append(tokens(token.toString))
        else if (token.toString == "")
          builder.append('@')
        else
          builder.append("@" + token.toString + "@")
      } else
        builder.append(char)
    }
    builder.toString
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
