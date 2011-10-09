import org.specs2.mutable._
import java.io.{File}
import scalaxb.compiler.Module
import scalaxb.compiler.xsd.{Driver}
import org.specs2.matcher

trait TestBase extends Specification with CompilerMatcher with matcher.FileMatchers {
  val module: Module = new scalaxb.compiler.xsd2.Driver // with Verbose
  val tmp = new File("tmp")
  if (tmp.exists) deleteAll(tmp)
  tmp.mkdirs() // you need this for copyFileFromResource
}
