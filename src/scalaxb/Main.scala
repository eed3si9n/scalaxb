package scalaxb

/** this class finds the module specified on command line and runs
 *  it with the arguments.
 */
object Main {

  var mname: String = null
  var args: List[String] = null
  var verbose = false

  def log(msg: String) {
    if (verbose) { Console.println("["+msg+"]"); Console.flush }
  }

  def main(args: Array[String]) {
    val z: List[String] = args.toList // convert with view

    processArgs(z)

    if (mname == null || args == null)
      System.exit(0)

    try {    // use reflection to get module

      //Class.forName("scalaxb.Module$class")
      //Class.forName("scalaxb.Module$ModuleConfig")
      val theClass = Class.forName(mname+"$")

      //log("main:theClass == '"+theClass+"'")

      val theModule = theClass.getField("MODULE$")
      val mod = theModule.get(null).asInstanceOf[Module]

      log("main:got scalaxb module '"+mname+"'")

      // pass arguments
      mod.getConfig().args = this.args

      // run
      mod.process()
    }
    catch {
      case e: Exception =>
        e.printStackTrace
        // Console.println(e.getMessage())
    }
  }

  def processArgs(z: List[String]) {
    z match {
      case ("--verbose" | "-v") :: rest =>
        verbose = true
        Console.println("main:processArgs verbose set")
        processArgs(rest)
      
      case "--module" :: name :: rest =>
        mname = name
        args  = rest
            
      case "xsd" :: rest =>
        mname = "scalaxb.xsd.Driver"
        args  = rest
      
      case _ =>
        Console.println("usage: ")
        Console.println("  scalaxb xsd [flags] --module mname arg* ")
        Console.println("\nwhere supported [flags] may be: ")
        Console.println("  --verbose    prints some debugging information")
    }
  }

}
