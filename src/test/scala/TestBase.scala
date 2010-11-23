import org.specs._
import java.io.{File}

trait TestBase extends SpecificationWithJUnit with CompilerMatcher {
  val module = new scalaxb.compiler.xsd.Driver
  val tmp = new File("tmp")
  val outProtocolFile = new File(tmp, "xmlprotocol.scala")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir
}
