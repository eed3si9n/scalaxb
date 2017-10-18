import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._
import io.Source

object EnumValuesTest extends TestBase {
  val schema = resource("enumTestSchema.xsd")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(Some("http://simple/main") -> Some("enumconflicts"), None -> Some("default")))).
      update(Outdir(tmp)))

  lazy val outEnumHasValues: Boolean =
    generated.find(fi => fi.getName.contains("enumTestSchema.scala")) match {
      case Some(enumFile) => Source.fromFile(enumFile).mkString
        .contains("val values: Seq[DummyType] = Seq(NilType, Macro, Foo)")
      case None => false
    }


  "Enum must have the right values list" in {
    outEnumHasValues must_== true
  }
}