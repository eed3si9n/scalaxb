import java.io.{File}
import scalaxb.compiler.{Config}

object NullNamespaceTest extends TestBase {
  val inFile  = new File("src/test/resources/nullnamespace.xsd")
  val outFile = new File(tmp, "nullnamespace.scala")
  
  lazy val generated = module.process(inFile, outFile, outProtocolFile,
    Config(packageNames = Map[Option[String], Option[String]]()) )
  
  "nullnamespace.scala file must compile so that Foo can be used" in {
    (List("import scalaxb.Scalaxb._",
      "import DefaultXMLProtocol._",
      "fromXML[Foo](<foo><bar>a</bar><baz>b</baz></foo>).toString"),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
