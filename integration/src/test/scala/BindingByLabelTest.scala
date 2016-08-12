import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

// Replicates a certain piece of Open Office Schema http://www.ecma-international.org/publications/standards/Ecma-376.htm
// Note how <m:tbl> is bound to CT_Table, though it is in the xsd:any position according to the schema
object BindingByLabelTest extends TestBase {
  val schema = resource("bindingByLabelTestSchema.xsd")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(PackageNames(Map(None -> Some("bindingbylabel")))).
      update(Outdir(tmp)))

  "Nodes under xsd:any should be bound to DOM classes based on their label" in {
    (List(
      "import bindingbylabel._"
    , """scalaxb.fromXML[bindingbylabel.CT_GraphicalObject](
          <m:graphic xmlns:m="http://simple/main">
            <m:graphicData>
              <m:tbl><m:name>Foo</m:name></m:tbl>
            </m:graphicData>
          </m:graphic>
        ).toString"""),
     generated) must evaluateTo("CT_GraphicalObject(Some(CT_GraphicalObjectData(List(DataRecord({http://simple/main}tbl,CT_Table(Some(Foo)))))))", outdir = "./tmp")
  }
}
