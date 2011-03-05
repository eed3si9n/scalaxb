import java.io.{File}
import scalaxb.compiler.{Config}

object CrossNamespaceTest extends TestBase {
  val ipoxsd    = new File("integration/src/test/resources/ipo.xsd")
  val xnxsd = new File("integration/src/test/resources/xn.xsd")

  lazy val generated = module.processFiles(
    List(ipoxsd, xnxsd),
    Config(packageNames = Map(None -> Some("ipo") ),
      outdir = tmp) )

  "xn.scala must compile so that CrossNamespaceChoice can be used" in {
    (List("""scalaxb.fromXML[ipo.CrossNamespaceChoice](<xn:foo xmlns:xn="http://www.example.com/xn">
      <xn:human>
        <xn:name/>
      </xn:human>
    </xn:foo>).toString"""),
     generated) must evaluateTo("CrossNamespaceChoice(DataRecord({http://www.example.com/xn}human,Human()))", outdir = "./tmp")
  }
}
