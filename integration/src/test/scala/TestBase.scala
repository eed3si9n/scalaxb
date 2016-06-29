package scalaxb.specs

import org.specs2.mutable._
import java.io.{File}
import scalaxb.compiler._
import scalaxb.compiler.xsd2.{Driver => Driver2}
import org.specs2.matcher

trait TestBase extends Specification with CompilerMatcher with matcher.FileMatchers {
  Log.configureLogger(true)
  val module: Module = new Driver2
  val tmp = new File("tmp")
  if (tmp.exists) deleteAll(tmp)
  tmp.mkdirs() // you need this for copyFileFromResource
}
