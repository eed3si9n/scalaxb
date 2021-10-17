import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

object CanWriteDefaultLabelTest extends TestBase {
  private val schemaFile = resource("can_write_default_label.xsd")

  private def generate() = {
    val config = Config.default.update(Outdir(tmp)).update(PackageNames(Map(None -> Some("defaultLabels"))))
    module.process(schemaFile, config)
  }

  "XML generation falls back to default labels if none are specified" >> {
    repl(generate())(
      s"""
         import defaultLabels._
         import scalaxb.DataRecord

         val urgency = Urgency()
         val docId = Docid()
         val options = Seq(DataRecord(urgency), DataRecord(None, Some("customDocIdTag"), docId))
         scalaxb.toXML(Docdata(options), "docdata", scala.xml.TopScope).toString
       """, expectedResult = scala.xml.Utility.trim(
        <docdata>
          <urgency />
          <customDocIdTag />
        </docdata>).toString)
  }
}
