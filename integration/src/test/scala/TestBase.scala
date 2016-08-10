import org.specs2.mutable._
import java.io.{File}
import scalaxb.compiler.Module
import scalaxb.compiler.xsd.{Driver}
import org.specs2.matcher

trait TestBase extends Specification with CompilerMatcher with matcher.FileMatchers {
  val module: Module = new Driver // with Verbose
  val tmp = new File("tmp")
  def resource(name: String) = new File(s"integration/src/test/resources/$name")  

  if (tmp.exists) deleteAll(tmp)
  tmp.mkdirs() // you need this for copyFileFromResource
}
