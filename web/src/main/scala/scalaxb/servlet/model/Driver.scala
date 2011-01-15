package scalaxb.servlet.model

import java.io.{StringWriter, PrintWriter}
import scalaxb.compiler.{Config, CanBeReader, CanBeWriter}

object Driver {
  def process(files: Seq[SchemaFile], config: Config) = {
    implicit val fileReader = new CanBeReader[SchemaFile] {
      override def toReader(value: SchemaFile) = value.reader
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
