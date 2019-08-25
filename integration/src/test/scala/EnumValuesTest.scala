import org.specs2.execute.FailureException
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

import scala.io.Source

object EnumValuesTest extends TestBase {
  val schema = resource("enumTestSchema.xsd")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(Some("http://simple/main") -> Some("enumconflicts"), None -> Some("default")))).
      update(Outdir(tmp)))

  lazy val fileContent: String = generated.find(fi => fi.getName.contains("enumTestSchema.scala")) match {
    case Some(enumFile) =>
      Source.fromFile(enumFile).mkString
    case None => throw FailureException(failure("Could not find generated file: enumTestSchema.scala"))
  }

  "Enum must have the right values list" in {
    fileContent must contain("val values: Seq[DummyType] = Seq(NilType, Macro, Foo)")
  }

  "Enum must handle values starting with number correctly" in {
    fileContent must contain("val values: Seq[NumberType] = Seq(Number01, Number02, Number11, Number12)")
  }
}
