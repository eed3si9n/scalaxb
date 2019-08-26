import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

class AutoPackagesTest extends TestBase {
  val ns      = "http://simple/main"

  val schema = resource("anotherSimpleSchema.xsd")
  val targetXml =
    (<n:aElem xmlns:n={ns}>
      <n:bElem>
        <n:cElem>Foo</n:cElem>
      </n:bElem>
      <n:bElem>
        <n:cElem>Bar</n:cElem>
      </n:bElem>
      <n:bElem>
        <n:cElem>Boo</n:cElem>
      </n:bElem>
    </n:aElem>).toString.replace("\n", "")

  lazy val generated = module.processFiles(
    List(schema),
    Config.default
      .update(AutoPackages)
      .update(Outdir(tmp))
  )

  "When AutoPacakges flag is set, the package names should be generated automatically from the namespace URI" in repl(generated)(s"""
    import simple.main._

    val elem = scalaxb.fromXML[AElem]($targetXml)
    "true"
  """,
  """true"""
  )

}
