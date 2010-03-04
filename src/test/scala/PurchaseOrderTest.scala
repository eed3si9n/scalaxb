import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

class PurchaseOrderTest extends Specification with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val ipoxsd = new File("src/test/resources/ipo.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    tmp.delete
  tmp.mkdir
  
  val iposcala = new File(tmp, "ipo.scala")
      
  "ipo.xsd is parsable" in {
     module.parse(ipoxsd) must be like {
       case schema: SchemaDecl => true
     }
  }
  
  lazy val generated = module.process(ipoxsd, iposcala, Some("ipo"))
  "ipo.xsd must generate ipo.scala file" in {
    generated must exist
  }
  
  "ipo.xsd must ipo.scala that compiles" in {
    List(generated) must compile(outdir = "./tmp")
  }
  
  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
          "Address(\"\", \"\", \"\").toString"), 
     List(generated)) must evaluateTo("Address(,,)", outdir = "./tmp")
  }
}
