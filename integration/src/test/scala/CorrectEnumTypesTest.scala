import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object CorrectEnumTypesTest extends TestBase {
  val schema = resource("correctEnumTestSchema.xsd")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(None -> Some("enumtypes")))).
      update(Outdir(tmp)))

  "00 should be the same thing as 0 if the type is byte" in {
    (List(
      "import enumtypes._"
    , """scalaxb.fromXML[enumtypes.Holder](
          <xn:holder xmlns:xn="http://simple/main" dummy="0"/>
        ).toString"""),
     generated) must evaluateTo("Holder(Map(@dummy -> DataRecord(00)))", outdir = "./tmp")
  }
}
