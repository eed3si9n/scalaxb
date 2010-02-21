/**
 * @author  e.e d3si9n
 */
 
package scalaxb.xsd

import scalaxb.{Module, Main}
import scala.collection.Map
// import scala.xml.xsd.{ElemDecl, TypeDecl}

object Driver extends Module {

  // %% instance variables, values

  var __theConfig: XsdConfig = null

  class XsdConfig extends super.ModuleConfig {
    var namespace: String = _
    var packageName: String = _
    
    def conf = this //getConfig();    
  }

  // %% type members

  type Config = XsdConfig
  type Schema = SchemaDecl

  // %% methods

  def generate(xsd: Schema) {
    Main.log("xsd: generating ...")
    getConfig() prepareOut ;
    Main.log("xsd: prepared output file")
    Main.log("xsd: running transform ...")
    new GenSource(getConfig(), xsd) run; 
    getConfig() finalizeOut; 
    Main.log("xsd: generating done.")
  }

  def getConfig(): XsdConfig = {
    if (__theConfig == null) __theConfig = new XsdConfig;
    __theConfig 
  }       

  def parse(): Schema = {
    val sysID = getConfig().sysID
    Main.log("xsd: parsing " + sysID)
    val schema = SchemaDecl.fromXML(scala.xml.XML.load(sysID))
    Main.log("SchemaParser.parse: " + schema.toString())
    schema
  }

  def printUsage {
    import Console.{ println => say }

    say("usage: ... [-d <dir>] [-p <package>] <sysID> <object name>")
    say("       binds a XSD to scala class definitions")
    say("       will create a file [<dir>/]<object name>.scala")
    say("<dir> is the output directory [path of <sysID> is default]")
    say("<package> is the package name")
    say("<sysID> is a system ID of an XML DTD")
    say("<object name> is the name of the resulting Scala source file ")
  }

  /** handles command line arguments */
  override def process() = {
    val conf = getConfig()
    Main.log("xsd: got config")
    conf.args = conf.args.toList  // (convenient)

    conf.args match {                           //- explicit outdir given?
      case "-d" :: outdir :: rest =>
        conf.outdir = new java.io.File(outdir)
        conf.args = rest
      case _ =>
    }

    conf.args match {
      case "-p" :: packageName :: rest =>
        conf.packageName = packageName
        conf.args = rest
      case _ =>
    }
    
    conf.args match {
      case sysID :: objName :: rest =>
        conf.sysID   = sysID
        conf.objName = objName
        conf.args = rest
      case _ =>
        println("Cannot find sysID and objName " + conf.args)
        printUsage;
        System.exit(-1) 
    }

    conf.args match {
      case nspace :: Nil =>
        conf.namespace = nspace
      case Nil =>
      case _  =>
        println("Cannot find Nil " + conf.args)
        printUsage;
        System.exit(-1) 
    }

    Main.log("xsd: parsed args")
    super.process()
  }
  
  /** setter method for the config */
  def setConfig(config: Config) {
    __theConfig = config
  }

  /** should report errors as side effect */
  def verifySchema(sd: Schema): Boolean = 
    true

  /** should report errors as side effect */
  def verifyOutput(): Boolean =
    true

}
