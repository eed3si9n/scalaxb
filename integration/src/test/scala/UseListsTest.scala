import org.specs2.execute.FailureException
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

import scala.io.Source

class UseListsTest extends TestBase {
  val schema = resource("useLists.xsd")

  lazy val generatedWithSeqs = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(Some("http://simple/main") -> Some("uselists"), None -> Some("default")))).
      update(Outdir(tmp)))
  
  lazy val generatedWithLists = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(Some("http://simple/main") -> Some("uselists"), None -> Some("default")))).
      update(Outdir(tmp)).
      update(UseLists))
  
  lazy val fileContentWithSeqs: String = generatedWithSeqs.find(fi => fi.getName.contains("useLists.scala")) match {
    case Some(enumFile) =>
      Source.fromFile(enumFile).mkString
    case None => throw FailureException(failure("Could not find generated file: useLists.scala"))
  }

  lazy val fileContentWithLists: String = generatedWithLists.find(fi => fi.getName.contains("useLists.scala")) match {
    case Some(enumFile) =>
      Source.fromFile(enumFile).mkString
    case None => throw FailureException(failure("Could not find generated file: useLists.scala"))
  }
  
  "Use Seq[T] by default" in {
    fileContentWithSeqs must contain("Seq[")
    fileContentWithSeqs must not(contain("List["))
  }

  "Use List[T] when configured" in {
    fileContentWithLists must contain("List[")
    fileContentWithLists must not(contain("Seq["))
  }
}
