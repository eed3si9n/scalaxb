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
    Map[String, Option[String]](),
    None
      )
  
  "xhtml1-strict.scala file must compile so that Html can be used" in {
    (List("""Html.fromXML(
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <base href="here.com"/>
    <title>a basic xhtml page</title>
  </head>
  <body>
    <h1>Welcome to xhtml in scala</h1>
    <p>a paragraph</p>
    <p>another, with a <a href="http://scalaxb.org/">example link</a>.</p>
  </body>
</html>).toString"""),
     generated) must evaluateTo("Foo(a,b)", outdir = "./tmp")
  }
}
