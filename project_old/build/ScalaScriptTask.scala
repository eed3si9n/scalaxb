import sbt._
import java.io.{File, InputStream, FileWriter}

trait ScalaScriptTask extends DefaultProject {    
  def outputBinPath = (outputPath ##) / "bin"
  def scriptName = name
  def scriptPath = outputBinPath / scriptName
  def scriptClasspath: List[String] = Nil
  def scriptProperties: List[Tuple2[String, String]] = Nil
  def scriptJavaFlags = "-Xmx256M -Xms16M"
  def scriptToolFlags = ""
  
  lazy val scalascript = scalascriptTask(scriptPath.asFile,
    scriptClasspath, scriptProperties, scriptJavaFlags, scriptToolFlags)
  
  /** generates shell script to execute the program.
   * uses <code>mainClassValue</code> and <code>runClasspath</code>.
   */
  def scalascriptTask(file: File,
      classpath: List[String],
      properties: List[Tuple2[String, String]],
      javaFlags: String,
      toolFlags: String) = task {        
    
    val props = properties.map({
      case Pair(name, value) => "-D" + name + "=\"" + value + "\""
    }).mkString("", " ", "")
    
    val mainClassValue = getMainClass(true) match {
      case Some(x) => x
      case None => error("mainClass was not found.")
    }
        
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
    val CRLF = "\r\n"
    val LF = "\n"
    
    if (platforms.contains("unix")) {
      val unixclasspath = processTokens(classpath.mkString("", ":", "").replace('\\', '/')) {
        case ""    => "@"
        case token => "${" + token + "}"
      }  
      
      val tokens = patches + ("classpath" -> unixclasspath)
      val unixTemplate = processTokens(unixTemplateString) {
        case token if tokens contains token => tokens(token)
        case "" => "@"
        case token => "@" + token + "@"
      }
      writeFile(file, unixTemplate, LF)
    }
    
    if (platforms.contains("windows")) {
      val winclasspath = processTokens(classpath.mkString("", ";", "").replace('/', '\\')) {
        case ""    => "@"
        case token => "%" + token + "%"
      }
      
      val tokens = patches + ("classpath" -> winclasspath)
      val winTemplate = processTokens(winTemplateString) {
        case token if tokens contains token => tokens(token)
        case "" => "@"
        case token => "@" + token + "@"
      }
      writeFile(new File(file.getAbsolutePath() + ".bat"), winTemplate, CRLF)
    }
    None
  }
  
  private def processTokens(text: String)(f: String => String) : String = {
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
        
        builder append f(token.toString)
      }
      else builder.append(char)          
    }
    builder.toString
  }
  
  private def writeFile(file: File, content: String, newline: String) =
    if (file.exists() && !file.canWrite()) error("File " + file + " is not writable")
    else {
      val writer = new FileWriter(file, false)
      val chars = scala.io.Source.fromString(content)
      for (line <- chars.getLines) writer.write(line.stripLineEnd + newline)
      writer.close()
    }
  
  private val unixTemplateString = """#!/bin/sh
#
##############################################################################
# Copyright 2002-2010, LAMP/EPFL
#
# This is free software; see the distribution for copying conditions.
# There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.
##############################################################################

cygwin=false;
case "`uname`" in
    CYGWIN*) cygwin=true ;;
esac

# Finding the root folder for this Scala distribution
SOURCE=$0;
SCRIPT=`basename "$SOURCE"`;
while [ -h "$SOURCE" ]; do
    SCRIPT=`basename "$SOURCE"`;
    LOOKUP=`ls -ld "$SOURCE"`;
    TARGET=`expr "$LOOKUP" : '.*-> \(.*\)$'`;
    if expr "${TARGET:-.}/" : '/.*/$' > /dev/null; then
        SOURCE=${TARGET:-.};
    else
        SOURCE=`dirname "$SOURCE"`/${TARGET:-.};
    fi;
done;

# see #2092
SCALA_HOME=`dirname "$SOURCE"`
SCALA_HOME=`cd "$SCALA_HOME"; pwd -P`
SCALA_HOME=`cd "$SCALA_HOME"/..; pwd`

# Remove spaces from SCALA_HOME on windows
if $cygwin; then
    SCALA_HOME=`cygpath --windows --short-name "$SCALA_HOME"`
    SCALA_HOME=`cygpath --unix "$SCALA_HOME"`
fi

# Constructing the extension classpath
TOOL_CLASSPATH="@classpath@"
if [ -z "$TOOL_CLASSPATH" ] ; then
    for ext in "$SCALA_HOME"/lib/* ; do
        if [ -z "$TOOL_CLASSPATH" ] ; then
            TOOL_CLASSPATH="$ext"
        else
            TOOL_CLASSPATH="$TOOL_CLASSPATH:$ext"
        fi
    done
fi

if $cygwin; then
    if [ "$OS" = "Windows_NT" ] && cygpath -m .>/dev/null 2>/dev/null ; then
        format=mixed
    else
        format=windows
    fi
    SCALA_HOME=`cygpath --$format "$SCALA_HOME"`
    TOOL_CLASSPATH=`cygpath --path --$format "$TOOL_CLASSPATH"`
fi

# Reminder: substitution ${JAVA_OPTS:=-Xmx256M -Xms16M} DO NOT work on Solaris
[ -n "$JAVA_OPTS" ] || JAVA_OPTS="@javaflags@"

# break out -D options and add them to JAVA_OPTS as well so they reach the
# underlying JVM in time to do some good.
for i
do
  case "$i" in
    -D*)
      JAVA_OPTS="$JAVA_OPTS $i" ;;
    *)
      ;;
  esac
done 


if [ -z "$JAVACMD" -a -n "$JAVA_HOME" -a -x "$JAVA_HOME/bin/java" ]; then
    JAVACMD="$JAVA_HOME/bin/java"
fi

exec "${JAVACMD:=java}" $JAVA_OPTS -cp "$TOOL_CLASSPATH" -Dscala.home="$SCALA_HOME" -Denv.emacs="$EMACS" @properties@ @class@ @toolflags@ "$@@"
"""
  
  private val winTemplateString = """@@echo off

rem ##########################################################################
rem # Copyright 2002-2010, LAMP/EPFL
rem #
rem # This is free software; see the distribution for copying conditions.
rem # There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
rem # PARTICULAR PURPOSE.
rem ##########################################################################

rem We adopt the following conventions:
rem - System/user environment variables start with a letter
rem - Local batch variables start with an underscore ('_')

if "%OS%"=="Windows_NT" (
  @@setlocal
  call :set_home
  set _ARGS=%*
) else (
  set _SCALA_HOME=%SCALA_HOME%
  rem The following line tests SCALA_HOME instead of _SCALA_HOME, because
  rem the above change to _SCALA_HOME is not visible within this block.
  if "%SCALA_HOME%"=="" goto error1
  call :set_args
)

rem We use the value of the JAVACMD environment variable if defined
set _JAVACMD=%JAVACMD%

if "%_JAVACMD%"=="" (
  if not "%JAVA_HOME%"=="" (
    if exist "%JAVA_HOME%\bin\java.exe" set _JAVACMD=%JAVA_HOME%\bin\java.exe
  )
)

if "%_JAVACMD%"=="" set _JAVACMD=java

rem We use the value of the JAVA_OPTS environment variable if defined
set _JAVA_OPTS=%JAVA_OPTS%
if "%_JAVA_OPTS%"=="" set _JAVA_OPTS=@javaflags@

set _TOOL_CLASSPATH=@classpath@
if "%_TOOL_CLASSPATH%"=="" (
  for %%f in ("%_SCALA_HOME%\lib\*") do call :add_cpath "%%f"
  if "%OS%"=="Windows_NT" (
    for /d %%f in ("%_SCALA_HOME%\lib\*") do call :add_cpath "%%f"
  )
)

set _PROPS=-Dscala.home="%_SCALA_HOME%" -Denv.emacs="%EMACS%" @properties@

rem echo "%_JAVACMD%" %_JAVA_OPTS% %_PROPS% -cp "%_TOOL_CLASSPATH%" @class@ @toolflags@ %_ARGS%
"%_JAVACMD%" %_JAVA_OPTS% %_PROPS% -cp "%_TOOL_CLASSPATH%" @class@ @toolflags@ %_ARGS%
goto end

rem ##########################################################################
rem # subroutines

:add_cpath
  if "%_TOOL_CLASSPATH%"=="" (
    set _TOOL_CLASSPATH=%~1
  ) else (
    set _TOOL_CLASSPATH=%_TOOL_CLASSPATH%;%~1
  )
goto :eof

rem Variable "%~dps0" works on WinXP SP2 or newer
rem (see http://support.microsoft.com/?kbid=833431)
rem set _SCALA_HOME=%~dps0..
:set_home
  set _BIN_DIR=
  for %%i in (%~sf0) do set _BIN_DIR=%_BIN_DIR%%%~dpsi
  set _SCALA_HOME=%_BIN_DIR%..
goto :eof

:set_args
  set _ARGS=
  :loop
  rem Argument %1 may contain quotes so we use parentheses here
  if (%1)==() goto :eof
  set _ARGS=%_ARGS% %1
  shift
  goto loop

rem ##########################################################################
rem # errors

:error1
echo ERROR: environment variable SCALA_HOME is undefined. It should point to your installation directory.
goto end

:end
if "%OS%"=="Windows_NT" @@endlocal
"""
}
