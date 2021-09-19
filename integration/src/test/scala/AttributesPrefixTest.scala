
import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

class AttributesPrefixTest extends TestBase {
  val inFile  = resource("attrPrefixSchema.xsd")

  lazy val generated = module.process(inFile,
    Config.default.update(PackageNames(Map(None -> Some("attr")))).
      update(Outdir(tmp)).
      update(AttributePrefix("a")))

  val targetXml =
    (<metricsPort id="metricsId" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <name>metricsName</name>
      <attributes>
        <attribute>
          <name>attr1</name>
          <value>val1</value>
        </attribute>
        <attribute>
          <name>attr2</name>
          <value>val2</value>
        </attribute>
      </attributes>
    </metricsPort>).toString.replace("\n", "")

  "When xsd has elements with name `attributes` and `attribute` " +
    "generated code should compile with config `AttributePrefix`" in repl(generated)(s"""
    import attr._

    val metricsPort = scalaxb.fromXML[MetricsPort]($targetXml)
    "true"
  """,
    """true"""
  )
}
