import java.io.{File}
import scalaxb.compiler.Config

object BigTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/big.xsd")
  // override val module = new scalaxb.compiler.xsd.Driver with Verbose
  lazy val generated = module.process(inFile,
    new Config(packageNames = Map(None -> Some("big") ),
      outdir = tmp,
      classPrefix = Some("X"),
      paramPrefix = Some("m_"),
      wrappedComplexTypes = List("barOne")) )
    
  "big.scala file must compile so that Foo can be used" in {
    (List("val subject = <foo xmlns:o=\"http://www.example.com/other\">" +
    "<string1></string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><o:string30></o:string30>" +
    """</foo>""",
    """scalaxb.toXML[big.XFoo](scalaxb.fromXML[big.XFoo](subject), None, Some("foo"), subject.scope).toString"""),
     generated) must evaluateTo("<foo xmlns:o=\"http://www.example.com/other\">" +
    "<string1></string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><o:string30></o:string30>" +
    "</foo>", outdir = "./tmp")
  }
  
  "big.scala file must compile so that XBaz can be used" in {
    (List("import scalaxb._",
      "import big._",
      "toXML[big.XBaz](fromXML[big.XBaz](<foo>" +
    "<string1>123</string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><string30></string30>" +
    """</foo>), None, Some("foo"), scala.xml.TopScope).toString"""),
     generated) must evaluateTo("<foo>" +
    "<string1>123</string1><string2></string2><string3></string3><string4></string4><string5></string5>" +
    "<string6></string6><string7></string7><string8></string8><string9></string9><string10></string10>" + 
    "<string11></string11><string12></string12><string13></string13><string14></string14><string15></string15>" + 
    "<string16></string16><string17></string17><string18></string18><string19></string19><string20></string20>" +
    "<string21></string21><string22></string22><string23></string23><string24></string24><string25></string25>" + 
    "<string26></string26><string27></string27><string28></string28><string29></string29><string30></string30>" +
    "</foo>", outdir = "./tmp")
  }
}
