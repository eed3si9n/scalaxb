import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

class VisitorTest extends TestBase {
  val schema = resource("anotherSimpleSchema.xsd")
  val targetXml =
    (<n:aElem xmlns:n="http://simple/main">
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
    import visitortest._
    import scalaxb.Visitor

    val elem = scalaxb.fromXML[AElem]($targetXml)
  """


  lazy val generated = module.processFiles(
    List(schema),
    Config.default.update(GenerateVisitor).update(PackageNames(Map(None -> Some("visitortest")))).
      update(Outdir(tmp)))

  "Visitor should have a working collect method" in repl(generated)(s"""
    $setup

    val allStrings = Visitor(elem).collect[String]
    val allBElems  = Visitor(elem).collect[BElem ]
    (allStrings :+ allBElems.size).toString
    """,
    """List(Foo, Bar, Boo, 3)"""
  )

  "Visitor should have a working visiting capability" in repl(generated) (s"""
    $setup

    var i = 0
    Visitor(elem).visit {
      case x: BElem => i += 1
    }
    i.toString
  """,
  "3")

  "Visitor should be capable to skip children on demand" in repl(generated) (s"""
    $setup

    var i = 0
    Visitor(elem) {v => {
      case x: String          => i += 1
      case x: BElem if i >= 2 => v.skipChildren
    }}
    i.toString
  """,
  "2")

}
