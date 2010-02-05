/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: Main.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src

/** this class finds the module specified on command line and runs
 *  it with the arguments.
 */
object Main {

  var mname: String = null
  var args: Seq[String] = null
  var verbose = false

  def log(msg: String) {
    if (verbose) { Console.println("["+msg+"]"); Console.flush }
  }

  def main(args: Array[String]) {
    val z: Seq[String] = args // convert with view

    processArgs(z)

    if (mname == null || args == null)
      System.exit(0)

    try {    // use reflection to get module

      //Class.forName("schema2src.Module$class")
      //Class.forName("schema2src.Module$ModuleConfig")
      val theClass = Class.forName(mname+"$")

      //log("main:theClass == '"+theClass+"'")

      val theModule = theClass.getField("MODULE$")
      val mod = theModule.get(null).asInstanceOf[Module]

      log("main:got schema2src module '"+mname+"'")

      // pass arguments
      mod.getConfig().args = this.args

      // run
      mod.process()
    }
    catch {
      case e: Exception =>
        Console.println(e.getMessage())
    }
  }

  def processArgs(z: Seq[String]) {
    z match {
      case Seq("--verbose", rest @ _*) =>
        this.verbose = true
        processArgs(rest)
      case Seq("--module", mname, rest @ _*) =>
        this.mname = mname
        this.args  = rest
      case Seq("dtd", rest @ _*) =>
        this.mname = "schema2src.dtd.Driver"
        this.args  = rest
      case Seq("xsd", rest @ _*) =>
        this.mname = "schema2src.xsd.Driver"
        this.args  = rest
      case _ =>
        Console.println("usage: ")
        Console.println("  schema2src [flags] --module mname arg* ")
        Console.println("or")
        Console.println("  schema2src dtd arg* ")
        Console.println("or (experimental)")
        Console.println("  schema2src xsd arg* (this doesn't work at all yet)")
        Console.println("\nwhere supported [flags] may be: ")
        Console.println("  --verbose    prints some debugging information")
    }
  }

}
