/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

import org.specs.matcher.Matcher
import java.io.{File, PrintWriter, BufferedWriter, BufferedReader, StringReader, OutputStreamWriter}
import scala.tools.nsc.{Global, Interpreter, Settings, GenericRunnerSettings}
import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.io.{PlainFile} 
import scala.tools.nsc.reporters.{ConsoleReporter}

class Holder {
  var value: Any = _
  
  override def toString: String = value match {
    case s: String => s
    case ref: AnyRef => ref.toString
    case _ => super.toString
  }
  
}

trait CompilerMatcher {  
  private lazy val bootPathList = List(jarPathOfClass("scala.tools.nsc.Interpreter"),
                                   jarPathOfClass("scala.ScalaObject"))
  
  /** evaluteTo matches a pair of code list and files against the expected value 
   * after evaluating the files and the given code list.
   * @param expected: Any - expected value
   * @param outdir: String - output dir for the interpreter
   * 
   * <code>
   * (List("import ipo._",
   *       "Address(\"\", \"\", \"\").toString"), 
   *   List(generated)) must evaluateTo("Address(,,)")
   * </code>
   */
  def evaluateTo(expected: Any,
      outdir: String = ".",
      unchecked: Boolean = true) = new Matcher[(Seq[String], Seq[File])] {
    
    /** @param pair :=> (code: Seq[String], files: Seq[File])
     */
    def apply(pair: => (Seq[String], Seq[File])) = {
      try {
        val code = pair._1
        val files = pair._2
        
        if (code.size < 1)
          error("At least one line of code is required.")
        
        val holder = new Holder
        val classpathList = List(jarPathOfClass(holder.getClass.getName))
        
        val in = new BufferedReader(new StringReader(""))
        val out = new PrintWriter(new BufferedWriter(
          new OutputStreamWriter(System.out)))        
        val settings = new GenericRunnerSettings(out.println _)
        val origBootclasspath = settings.bootclasspath.value
        
        settings.bootclasspath.value = 
          (origBootclasspath :: bootPathList).mkString(java.io.File.pathSeparator)
        
        val originalClasspath = settings.classpath.value
        settings.classpath.value =
          classpathList.mkString(java.io.File.pathSeparator)
        settings.outdir.value = outdir
        settings.unchecked.value = unchecked
        
        val interpreter = new Interpreter(settings)
        interpreter.bind("$r_", holder.getClass.getName , holder)
        interpreter.compileSources(files.map(toSourceFile(_)): _*)
        
        code.take(code.size - 1).foreach(interpreter.interpret(_))
        val expression = code.last
        interpreter.interpret("$r_.value = " + expression)
        
        if (holder.value != expected)
          println("actual: " + holder.toString)
        
        (holder.value == expected,
          code + " evaluates as expected",
          code + " does not evaluate as expected")        
      } catch {
        case e: Exception =>
          println(e.toString)
          e.printStackTrace
          throw(e)        
      }
    }    
  }
  
  /** compile checks if the given list of files compiles without an error.
   * @param outdir: String - output dir for the interpreter
   */
  def compile(outdir: String = ".") = new Matcher[Seq[File]]() {
    
    /** @param files :=> Seq[File]
     */
    def apply(files: => Seq[File]) = {
      val settings = new Settings
      val origBootclasspath = settings.bootclasspath.value
      settings.bootclasspath.value = 
        (origBootclasspath :: bootPathList).mkString(java.io.File.pathSeparator)
      settings.outdir.value = outdir
      
      val reporter = new ConsoleReporter(settings)
      val compiler = new Global(settings, reporter)
      
      try {
        val run = (new compiler.Run)
        run.compile(files.map(_.getAbsolutePath).toList)
        reporter.printSummary
        (!reporter.hasErrors,
          files.mkString + " compile(s)",
          files.mkString + " do(es) not compile")
      } catch {
        case e: Exception =>
          println(e.toString)
          // e.printStackTrace
          throw(e)
      }
    }
  }

  def deleteAll(file: File): Boolean = {
    if (file.isDirectory) {
      val children = file.listFiles
      if (children != null)
        children.foreach(deleteAll(_))
    }
    file.delete
  }

  def copyFileFromResource(source: String, dest: File) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.PrintWriter(new java.io.FileWriter(dest))
    var line: String = null
    line = reader.readLine
    while (line != null) {
      out.println(line)
      line = reader.readLine
    }
    in.close
    out.flush
  }
  
  private def jarPathOfClass(className: String) = {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:")
    val indexOfSeparator = path.lastIndexOf('!')
    if (indexOfFile == -1 || indexOfSeparator == -1) {
      val indexOfSlash = path.lastIndexOf('/')
      path.substring(0, indexOfSlash) 
    } else {
      path.substring(indexOfFile, indexOfSeparator)
    }
  }
  
  private def toSourceFile(file: File): SourceFile = 
    new BatchSourceFile(new PlainFile(file))
}
