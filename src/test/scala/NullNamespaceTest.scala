import java.io.{File}
import org.scalaxb.compiler.{Config}

object NullNamespaceTest extends TestBase {
  val inFile  = new File("src/test/resources/nullnamespace.xsd")
  val outFile = new File(tmp, "nullnamespace.scala")
  
  lazy val generated = module.process(inFile, outFile,
    Config(packageNames = Map[Option[String], Option[String]]()) )
  
  "nullnamespace.scala file must compile so that Foo can be used" in {
    (List("Foo.fromXML(<foo><bar>a</bar><baz>b</baz></foo>).toString"),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
