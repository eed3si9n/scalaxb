import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

class AnyAttributeDefaultValue extends TestBase {
  private val schemaFile = resource("any_attribute_default_value.xsd")

  private def generate() = {
    val config = Config.default.update(Outdir(tmp))
      .update(PackageNames(Map(None -> Some("anyAttributes"))))
      .update(NamedAttributes)
    module.process(schemaFile, config)
  }

  "Generated classes should offer a default value for the map of attributes when anyAttribute is included" >> {
    repl(generate())(
      s"""
         import anyAttributes._
         import scalaxb.DataRecord

         val x = HasAnyAttribute()  // parameter `attributes` should have a default value
         "success"
       """, expectedResult = "success")  // it just needs to compile
  }
}
