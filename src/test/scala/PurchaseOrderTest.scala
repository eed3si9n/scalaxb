import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object PurchaseOrderTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val ipoxsd = new File("src/test/resources/ipo.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir
  
  val iposcala = new File(tmp, "ipo.scala")
  val purchaseOrderUsagescala = new File(tmp, "PurchaseOrderUsage.scala")
  copyFileFromResource("PurchaseOrderUsage.scala", purchaseOrderUsagescala)

  "ipo.xsd is parsable" in {
     module.parse(ipoxsd) must be like {
       case schema: SchemaDecl => true
     }
  }

  lazy val generated = module.process(ipoxsd,
    iposcala,
    Some("ipo"),
    false)
  
  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
          "Address(\"\", \"\", \"\").toString"), 
     generated) must evaluateTo("Address(,,)", outdir = "./tmp")
  }

  "ipo.scala file must compile together with PurchaseOrderUsage.scala" in {
    (List("import ipo._",
          "PurchaseOrderUsage.allTests"),
     purchaseOrderUsagescala :: generated) must evaluateTo(true,
       outdir = "./tmp")
  }
}
