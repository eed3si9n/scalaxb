/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: Driver.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.dtd

import scala.xml._
import scala.xml.parsing.{ExternalSources, MarkupParser}
import schema2src._

/** main driver for the dtd module
 *  @author buraq
 */
object Driver extends Module {

  Main.log("instantiating Driver")
  // %% instance variables, values

  var __theConfig: DtdConfig = null

  // %% nested classes

  class DtdConfig extends super.ModuleConfig {
    var namespace: String = _

    def conf = this //getConfig();

  } // end DtdConfig

  // %% type members

  /* a record in which config info is stored */
  type Config = DtdConfig

  /* a data structure that represents the schema */
  type Schema = scala.collection.Map[String, MyElemDecl]

  // %% methods

  def generate(dtd: Schema) = {
    getConfig() prepareOut;
    new GenSource(getConfig(), dtd) run;
    getConfig() finalizeOut
  }

  def getConfig() = {
    if (__theConfig == null) __theConfig = new DtdConfig
    __theConfig 
  }       

  def parse(): Schema = {
    val sysID = getConfig().sysID
    val myH = new DtdHandler() with MarkupParser with ExternalSources {
      override val input = inputsrc(sysID)
      val preserveWS = false
      override def reportSyntaxError(p: Int, s: String) =
        input.reportError(p, s)
    }
    if (myH.curInput == null) {
      //Console.println("Scala sucks! should have init'ed curInput itself");
      myH.curInput = myH.input
    }
    //- initialize parser
    myH.nextch

    //- do parse
    myH.extSubset()

    myH.elemMap
  }

  def printUsage {
    import Console.{ println => say }

    say("usage: ... [ -d <dir> ] <sysID> <object name> [<namespace>]")
    say("       binds a DTD to scala class definitions")
    say("       will create a file [<dir>/]<object name>.scala")
    say("<dir> is the output directory [path of <sysID> is default]")
    say("<sysID> is a system ID of an XML DTD")
    say("<object name> is the name of the resulting Scala source file ")
  }

  /** handles command line arguments */
  override def process() = {
    val conf = getConfig()
    Main.log("dtd: got config")
    conf.args = conf.args.toList  // (convenient)

    conf.args match {                           //- explicit outdir given?
      case "-d" :: outdir :: rest =>
        conf.outdir = new java.io.File(outdir)
        conf.args = rest
      case _ =>
    }

    conf.args match {
      case sysID :: objName :: rest =>
        conf.sysID   = sysID
        conf.objName = objName
        conf.args = rest
      case _ =>
        printUsage; System.exit(-1) 
    }

    conf.args match {
      case nspace :: Nil =>
        conf.namespace = nspace
      case Nil =>
      case _  =>
        printUsage; System.exit(-1) 
    }

    Main.log("dtd: parsed args")
    super.process()
  }

  def setConfig(config: DtdConfig) = 
    this.__theConfig = config

  def verifySchema(sd: Schema) = 
    false

  def verifyOutput() = 
    false

}
