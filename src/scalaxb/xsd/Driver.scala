/**
 * @author  e.e d3si9n
 */
 
package scalaxb.xsd

import scalaxb.{Module, Main}
import scala.collection.Map
import java.io.{File, FileWriter, PrintWriter}

object Driver extends Module {
  type Config = XsdConfig
  type Schema = SchemaDecl
  
  class XsdConfig extends super.ModuleConfig {

  }
    
  def generate(xsd: Schema, output: File) {
    val out = new PrintWriter(new FileWriter(output))
    Main.log("xsd: generating ...")
    new GenSource(config, xsd, out) run;
    out.flush()
    out.close()
    println("generated " + output)
  }

  lazy val config = new XsdConfig()       

  def parse(input: java.io.File): Schema = {
    Main.log("xsd: parsing " + input)
    val elem = scala.xml.XML.loadFile(input)
    val schema = SchemaDecl.fromXML(elem)
    Main.log("SchemaParser.parse: " + schema.toString())
    schema
  }
  
  def verifySchema(sd: Schema): Boolean = 
    true
  
  def verifyOutput(): Boolean =
    true
}
