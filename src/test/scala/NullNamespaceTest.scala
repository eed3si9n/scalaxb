import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object NullNamespaceTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val nullnamespacexsd    = new File("src/test/resources/nullnamespace.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val nullnamespacescala = new File(tmp, "nullnamespace.scala")
    
  lazy val generated = module.processFiles(
    List((nullnamespacexsd, nullnamespacescala)),
    Map[Option[String], Option[String]]())
    
  "nullnamespace.xsd must generate nullnamespace.scala file" in {
    generated(0) must exist
  }

  "nullnamespace.scala file must compile so that Foo can be used" in {
    (List("Foo.fromXML(<foo><bar>a</bar><baz>b</baz></foo>).toString"),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
