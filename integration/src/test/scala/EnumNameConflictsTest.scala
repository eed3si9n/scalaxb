import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object EnumNameConflictsTest extends TestBase {
  val schema = resource("enumTestSchema.xsd")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(Some("http://simple/main") -> Some("enumconflicts"), None -> Some("default")))).
      update(Outdir(tmp)))

  "nil in enum name should be prefixed" in {
    (List(
      "import enumconflicts.NilType"
    , "import enumconflicts.Macro_"
    , "import enumconflicts.Foo"
    , """ "true" """),
     generated) must evaluateTo("true", outdir = "./tmp")
  }
}