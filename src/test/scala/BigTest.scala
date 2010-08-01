import org.specs._
import java.io.{File}
import org.scalaxb.compiler.xsd.{SchemaDecl}

object BigTest extends SpecificationWithJUnit with CompilerMatcher {
  val module = org.scalaxb.compiler.xsd.Driver
  val bigxsd    = new File("src/test/resources/big.xsd")
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir

  val bigscala = new File(tmp, "big.scala")
    
  lazy val generated = module.processFiles(
    List((bigxsd, bigscala)),
    Map[String, Option[String]](
      (null, Some("big"))
      ),
    None
      )
    
  "big.scala file must compile so that Foo can be used" in {
    (List("big.Foo.toXML(big.Foo.fromXML(<foo>" +
    "<string1></string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><string30></string30>" +
    """</foo>), null, "foo", scala.xml.TopScope).toString"""),
     generated) must evaluateTo("<foo>" +
    "<string1></string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><string30></string30>" +
    "</foo>", outdir = "./tmp")
  }
}
