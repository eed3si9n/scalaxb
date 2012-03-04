import org.specs2.mutable._
import java.io.{File}
import scalaxb.compiler.Module
import scalaxb.compiler.xsd.{Driver}

trait TestBase extends Specification with CompilerMatcher {
  val module: Module = new Driver // with Verbose
  val tmp = new File("tmp")
  if (tmp.exists) deleteAll(tmp)
  tmp.mkdirs() // you need this for copyFileFromResource
}
