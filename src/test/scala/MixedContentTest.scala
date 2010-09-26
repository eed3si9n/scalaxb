import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object MixedContentTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val mixedxsd    = new File("src/test/resources/mixed.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val mixedscala = new File(tmp, "mixed.scala")
  
  lazy val generated = module.process(mixedxsd,
    mixedscala,
    Some("mixed"),
    true)
  val mixedUsagescala = new File(tmp, "MixedUsage.scala")
  copyFileFromResource("MixedUsage.scala", mixedUsagescala)
    
  "mixed.scala file must compile together with MixedUsage.scala" in {
    (List("MixedUsage.allTests"),
      mixedUsagescala :: generated) must evaluateTo(true,
      outdir = "./tmp")
    // (mixedUsagescala :: generated) must compile(outdir = "./tmp")
  }
}
