/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: Module.scala 13960 2008-02-12 17:49:29Z michelou $
 
package scalaxb

import java.io.{File, FileWriter, PrintWriter}

trait Module {

  // type members
  type Config <: ModuleConfig

  type Schema

  class ModuleConfig {   // Config must at least have ...
    var args: Seq[String] = Nil

    var doVerifySchema = false
    var doVerifyOutput = false

    /** name of the object that is created as output */
    var objName: String = _

    /** output directory */
    var outdir: File = _

    /** the output file */
    var outfile: PrintWriter = _

    var src: scala.io.Source = null

    var sysID: String = _

    /** prepare output file */
    def prepareOut {
      //- ?default output directory
      if (null == outdir) outdir = new File(sysID).getParentFile()

      this.outfile = new PrintWriter(new FileWriter(new File(outdir, objName+".scala")))
    }

    /** finalize output file */
    def finalizeOut {
      outfile.flush()
      outfile.close()
      Console.println("generated " + outdir + java.io.File.separator + objName+".scala")
    }

  }

  // methods

  /** convenience - create input source from sysID
   */
  protected def inputsrc( sysID: String ) = 
    //    val curDir:String = System.getProperty("user.dir"); //@todo?
    scala.io.Source.fromPath(sysID)

  /** parse source into a schema representation */  
  def parse(): Schema

  /** generate code from schema representation */  
  def generate(sd: Schema): Unit

  /** getter method for the config */
  def getConfig(): Config

  /** what modules do */
  def process() {
    val conf = getConfig()

    import conf._

    val sd  = parse()
    var sdv = !(doVerifySchema) || verifySchema(sd);

    generate(sd)
    var outv = !(doVerifyOutput) || verifyOutput();
  }

  /** setter method for the config */
  def setConfig(config: Config): Unit

  /** should report errors as side effect */
  def verifySchema(sd: Schema): Boolean

  /** should report errors as side effect */
  def verifyOutput(): Boolean

}
