package scalaxb.servlet.model

import java.io.{StringWriter, PrintWriter}
import scalaxb.compiler.{Config, CanBeRawSchema, CanBeWriter, CustomXML}

object Driver {
  def process(files: Seq[SchemaFile], config: Config) = {
    implicit val fileReader = new CanBeRawSchema[SchemaFile, scala.xml.Node] {
      override def toRawSchema(value: SchemaFile) = CustomXML.load(value.reader)
      override def toURI(value: SchemaFile) = value.uri
    }
    implicit val fileWriter = new CanBeWriter[ScalaFile] {
      override def toWriter(value: ScalaFile) = value.printout
      override def newInstance(fileName: String) = new ScalaFile(fileName)
    }

    val module = new scalaxb.compiler.xsd.Driver

    module.processReaders(files, config)
  }
}
