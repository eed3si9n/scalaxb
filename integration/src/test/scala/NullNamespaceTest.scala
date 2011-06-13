import java.io.{File}
import scalaxb.compiler.{Config}

object NullNamespaceTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/nullnamespace.xsd")
  
  lazy val generated = module.process(inFile,
    Config(packageNames = Map[Option[String], Option[String]](),
      outdir = tmp) )
  
  "nullnamespace.scala file must compile so that Foo can be used" in {
    (List("scalaxb.fromXML[Foo](<foo><bar>a</bar><baz>b</baz></foo>).toString"),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
