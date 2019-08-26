import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

class MutableTest extends TestBase {
  val pkgname = "mutabletest"
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

  val setup = s"""
    import $pkgname._

    val elem = scalaxb.fromXML[AElem]($targetXml)
  """

  def genXml(elemName: String) = s"""
    scalaxb.toXML(
      obj = $elemName
    , namespace = Some("$ns")
    , elementLabel = Some("aElem")
    , scope = scalaxb.toScope(Some("n") -> "$ns")
    , typeAttribute = false
    )
  """


  lazy val generated = module.processFiles(
    List(schema),
    Config.default
      .update(PackageNames(Map(None -> Some(pkgname))))
      .update(Outdir(tmp))
      .update(GenerateMutable)
      .remove(VarArg)  // VarArg is removed automatically in the command line app
  )

  "It should be possible to modify the nodes" in repl(generated)(s"""
    $setup

    elem.bElem = Nil
    ${genXml("elem")}.toString
  """,
  """<n:aElem xmlns:n="http://simple/main"/>"""
  )

  "It should be possible to modify the attributes" in repl(generated)(s"""
    $setup

    elem.dummy = "bar"
    elem.bElem = Nil
    ${genXml("elem")}.toString
  """,
  """<n:aElem dummy="bar" xmlns:n="http://simple/main"/>"""
  )

}
