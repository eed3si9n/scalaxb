import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object XhtmlTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val xhtml1strictxsd = new File("src/test/resources/xhtml1-strict.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val xhtml1strictscala = new File(tmp, "xhtml1-strict.scala")
    
  lazy val generated = module.processFiles(
    List((xhtml1strictxsd, xhtml1strictscala)),
    Map[Option[String], Option[String]](
      Some("http://www.w3.org/1999/xhtml") -> Some("org.w3.xhtml")
    ), false)
  
  "xhtml1-strict.scala file must compile so that Html can be used" in {
    (List("import org.w3.xhtml._",
      """val document = <html xmlns="http://www.w3.org/1999/xhtml" lang="en">""" + // "
      """<head><title>foo</title></head><body></body></html>""",
      """Html.toXML(Html.fromXML(document),
        Some("http://www.w3.org/1999/xhtml"), Some("html"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<html lang="en" xmlns="http://www.w3.org/1999/xhtml">""" + // "
     """<head><title>foo</title></head><body></body></html>""", // "
     outdir = "./tmp")
  }
}
