import java.io.{File}
import scalaxb.compiler.{Config}
import scalaxb.compiler.ConfigEntry._

class NullNamespaceTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/nullnamespace.xsd")
  val config = Config.default.update(PackageNames(Map[Option[String], Option[String]]())).
      update(Outdir(tmp))
  lazy val generated = module.process(inFile, config)

  "nullnamespace.scala file must compile so that Foo can be used" in {
    (List("scalaxb.fromXML[Foo](<foo><bar>a</bar><baz>b</baz></foo>).toString"),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
