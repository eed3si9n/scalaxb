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

package scalaxb.specs

import org.specs2.matcher._
import java.io.{File}
import scala.tools.nsc.{Settings}
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
      classpath: List[String] = Nil,
      usecurrentcp: Boolean = false,
      unchecked: Boolean = true) = new Matcher[(Seq[String], Seq[File])] {

    def apply[A <: (Seq[String], Seq[File])](pair: Expectable[A]) = {
      import scala.tools.nsc.interpreter.{IMain, Results => IR}
      import scala.util.control.Exception._

      val code = pair.value._1
      val files = pair.value._2
        
      if (code.size < 1)
        sys.error("At least one line of code is required.")
      
      val s = settings(outdir, classpath, usecurrentcp, unchecked)
      val main = new IMain(s) {
        def lastReq = prevRequestList.last
      }
      main.compileSources(files.map(toSourceFile(_)): _*)
      code map { c => main.interpret(c) match {
        case IR.Error => sys.error("Error interpreting %s" format (c))
        case _ => 
      }}
      val holder = allCatch opt {
        main.lastReq.lineRep.call("$result")
      }
      if (holder != Some(expected))
        println("actual: %s" format(holder.map(_.toString).getOrElse{"None"}))
      
      result(holder == Some(expected),
        code + " evaluates as expected",
        code + " does not evaluate as expected",
        pair)
    }    
  }
  
  private def settings(outdir: String, classpath: List[String],
      usecurrentcp: Boolean, unchecked: Boolean) = {
    import java.io.{PrintWriter, BufferedWriter, BufferedReader, StringReader, OutputStreamWriter}
    import scala.tools.nsc.{GenericRunnerSettings}
    
    val currentcp = if (usecurrentcp) {
      java.lang.Thread.currentThread.getContextClassLoader match {
        case cl: java.net.URLClassLoader => cl.getURLs.toList map {_.toString}
        case _ => sys.error("classloader is not a URLClassLoader")
      }
    } else Nil
    val classpathList = classpath ++ currentcp
    val in = new BufferedReader(new StringReader(""))
    val out = new PrintWriter(new BufferedWriter(
      new OutputStreamWriter(System.out)))        
    val settings = new GenericRunnerSettings(out.println _)
    val origBootclasspath = settings.bootclasspath.value
    
    settings.bootclasspath.value = 
      (origBootclasspath :: bootPathList).mkString(java.io.File.pathSeparator)
    
    val originalClasspath = settings.classpath.value
    settings.classpath.value = classpathList.distinct.mkString(java.io.File.pathSeparator)
    settings.outdir.value = outdir
    settings.unchecked.value = unchecked
    settings    
  }
  
  /** compile checks if the given list of files compiles without an error.
   * @param outdir: String - output dir for the interpreter
   */
  def compile(outdir: String = ".",
    classpath: List[String] = Nil,
    usecurrentcp: Boolean = false,
    unchecked: Boolean = true) = new Matcher[Seq[File]]() {

    def apply[A <: Seq[File]](files: Expectable[A]) = {
      import scala.tools.nsc.{Global}
      
      val s = settings(outdir, classpath, usecurrentcp, unchecked)
      val reporter = new ConsoleReporter(s)
      val compiler = new Global(s, reporter)
      val run = (new compiler.Run)
      run.compile(files.value.map(_.getAbsolutePath).toList)
      reporter.printSummary
      result(!reporter.hasErrors,
        files.value.mkString + " compile(s)",
        files.value.mkString + " do(es) not compile",
        files)
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
