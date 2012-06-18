package scalaxb.specs

import java.io.{File}

object XhtmlTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/xhtml1-strict.xsd")
  lazy val generated = module.process(inFile, "xhtml", tmp)
  
  "xhtml1-strict.scala file must compile so that Html can be used" in {
    (List("import scalaxb._",
      "import xhtml._",
      """val document = <html xmlns="http://www.w3.org/1999/xhtml" lang="en">""" + // "
      """<head><title>foo</title></head><body></body></html>""",
      """toXML[Html](fromXML[Html](document),
        Some("http://www.w3.org/1999/xhtml"), Some("html"), document.scope).toString""" // "
     ),
     generated) must evaluateTo("""<html lang="en" xmlns="http://www.w3.org/1999/xhtml">""" + // "
     """<head><title>foo</title></head><body/></html>""", // "
     outdir = "./tmp")
  }
}
