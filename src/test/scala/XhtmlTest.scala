import java.io.{File}

object XhtmlTest extends TestBase {
  val inFile  = new File("src/test/resources/xhtml1-strict.xsd")
  val outFile = new File(tmp, "xhtml1-strict.scala")
  lazy val generated = module.process(inFile, outFile, "org.w3.xhtml")
  
  "xhtml1-strict.scala file must compile so that Html can be used" in {
    (List("import scalaxb._",
      "import Scalaxb._",
      "import org.w3.xhtml._",
      "import DefaultXMLProtocol._",
      """val document = <html xmlns="http://www.w3.org/1999/xhtml" lang="en">""" + // "
      """<head><title>foo</title></head><body></body></html>""",
      """toXML[Html](fromXML[Html](document),
        Some("http://www.w3.org/1999/xhtml"), Some("html"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<html lang="en" xmlns="http://www.w3.org/1999/xhtml">""" + // "
     """<head><title>foo</title></head><body></body></html>""", // "
     outdir = "./tmp")
  }
}
