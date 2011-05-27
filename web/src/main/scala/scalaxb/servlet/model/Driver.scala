package scalaxb.servlet.model

import scalaxb.compiler.{Config, CanBeRawSchema, CanBeWriter, CustomXML, Module}
import java.io.{File}

object Driver {
  def process(files: Seq[SchemaFile], config: Config) = {
    val module = Module.moduleByFileName(new File(files.head.uri.getPath), false)

    implicit val fileReader = new CanBeRawSchema[SchemaFile, scala.xml.Node] {
      override def toRawSchema(value: SchemaFile) = CustomXML.load(value.reader)
      override def toURI(value: SchemaFile) = value.uri
    }
    implicit val fileWriter = new CanBeWriter[ScalaFile] {
      override def toWriter(value: ScalaFile) = value.printout
      override def newInstance(packageName: Option[String], fileName: String) = new ScalaFile(fileName)
    }

    module match {
      case x: scalaxb.compiler.xsd.Driver    => x.processReaders(files, config)
      case x: scalaxb.compiler.wsdl11.Driver => x.processReaders(files, config)
    }
  }
}
