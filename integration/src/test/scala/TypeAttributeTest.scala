import java.io.File

class TypeAttributeTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/traitsTypeAttribute.wsdl")
  lazy val generated = module.process(inFile, "typeattribute", tmp)

  "traitsTypeAttribute.scala must properly add type attribute" in {
    (List("import scalaxb._",
      "import typeattribute._",
      """val request: ServiceRequestable = MyRequest(IP = Some("127.0.0.1"), ID = 123)""",
      """toXML(request, "request", defaultScope).toString"""
     ),
     generated) must evaluateTo(
      """<request xsi:type="MyRequest" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><IP>127.0.0.1</IP><ID>123</ID></request>""",
     outdir = "./tmp")
  }
}
