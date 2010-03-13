import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

class PurchaseOrderTest extends SpecificationWithJUnit with CompilerMatcher {
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
  
  lazy val generated = module.process(ipoxsd, iposcala, Some("ipo"))
  "ipo.xsd must generate ipo.scala file" in {
    generated must exist
  }
  
  "ipo.scala file must compile so Address can be used" in {
    (List("import ipo._",
          "Address(\"\", \"\", \"\").toString"), 
     List(generated)) must evaluateTo("Address(,,)", outdir = "./tmp")
  }

  "ipo.scala file must compile together with poTest.scala" in {
    (List("import ipo._",
          "PurchaseOrderUsage.allTests"),
     purchaseOrderUsagescala :: List(generated)) must evaluateTo(true,
       outdir = "./tmp")
  }

  def deleteAll(file: File): Boolean = {
    if (file.isDirectory) {
      val children = file.listFiles
      if (children != null)
        children.foreach(deleteAll(_))
    }
    file.delete
  }

  def copyFileFromResource(source: String, dest: File) {
    val in = getClass.getResourceAsStream(source)
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in))
    val out = new java.io.PrintWriter(new java.io.FileWriter(dest))
    var line: String = null
    line = reader.readLine
    while (line != null) {
      out.println(line)
      line = reader.readLine
    }
    in.close
    out.flush
  }
}
