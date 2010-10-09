import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object AnyContentTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val anyxsd    = new File("src/test/resources/any.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val anyscala = new File(tmp, "any.scala")
  
  lazy val generated = module.process(anyxsd,
    anyscala,
    Some("any"))
  val anyUsagescala = new File(tmp, "AnyUsage.scala")
  copyFileFromResource("AnyUsage.scala", anyUsagescala)
    
  "any.scala file must compile together with AnyUsage.scala" in {
    (List("AnyUsage.allTests"),
      anyUsagescala :: generated) must evaluateTo(true,
      outdir = "./tmp")
  }
}
