/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: Driver.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.xsd

import scala.collection.Map
import scala.xml.xsd.{ElemDecl, TypeDecl}

object Driver extends Module {

  // %% instance variables, values

  var __theConfig: XsdConfig = null

  class XsdConfig extends super.ModuleConfig {}

  // %% type members

  type Config = XsdConfig
  type Schema = Pair[Map[String,ElemDecl], Map[String,XsTypeSymbol]]

  // %% methods

  def generate(xsd: Schema) {
    Main.log("xsd: generating ...")
    getConfig() prepareOut ;
    Main.log("xsd: prepared output file")
    Main.log("xsd: running transform ...")
    new GenSource( getConfig(), xsd ) run; 
    getConfig() finalizeOut; 
    Main.log("xsd: generating done.")
  }

  def getConfig(): XsdConfig = {
    if (__theConfig == null) __theConfig = new XsdConfig;
    __theConfig 
  }       

  def parse(): Schema = {
    val sysID = getConfig().sysID
    new SchemaParser(inputsrc(sysID))
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
