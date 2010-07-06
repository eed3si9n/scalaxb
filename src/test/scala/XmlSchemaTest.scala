import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object XmlSchemaTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val xmlschemaxsd = new File("src/test/resources/xmlschema.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val xmlschemascala = new File(tmp, "XMLSchema.scala")
    
  lazy val generated = module.processFiles(
    List((xmlschemaxsd, xmlschemascala)),
    Map[String, Option[String]](
      ("http://www.w3.org/2001/XMLSchema", Some("org.w3.xmlschema"))
      ),
    None )
  
  // "XMLSchema.scala file must compile" in {
  //   generated must compile(outdir = "./tmp")
  // }
}
