package scalaxb.servlet.model

import java.io.{StringWriter, PrintWriter}
import scalaxb.compiler.{Config}

object Driver {
  def process(files: Seq[SchemaFile], config: Config) = {    
    val swProtocol = new StringWriter()
    val pwProtocol = new PrintWriter(swProtocol)
    val module = new scalaxb.compiler.xsd.Driver
    module.processReaders(files map {_.toTriplet}, pwProtocol, config)
    val generated = files.toList map { x => ScalaFile.fromURI(x.uri, x.out.toString) }
    val xmlProtocol = ScalaFile("xmlprotocol.scala", swProtocol.toString)
    val helper = ScalaFile.fromResource("/scalaxb.scala", "scalaxb.scala")
    
    generated ::: List(xmlProtocol, helper)
  }
}
