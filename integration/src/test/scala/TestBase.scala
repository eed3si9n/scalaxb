import org.specs._
import java.io.{File}
import scalaxb.compiler.{Verbose}
import scalaxb.compiler.xsd.{Driver}

trait TestBase extends SpecificationWithJUnit with CompilerMatcher {
  val module = new Driver // with Verbose
  val tmp = new File("tmp")
  if (tmp.exists) deleteAll(tmp)
}
