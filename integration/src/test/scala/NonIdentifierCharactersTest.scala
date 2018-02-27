import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

import scala.xml._

object NonIdentifierCharactersTest extends TestBase {
  private val schema = resource("non_identifier_characters.xsd")
  private val doubleUnderscoresSchema = resource("double_underscores.xsd")

  private def generate(symbolEncodingStrategy: SymbolEncoding.Strategy,
                       schemaFile: File = schema) = {
    var config = Config.default.update(Outdir(tmp))
    config = config.update(symbolEncodingStrategy)
    module.process(schemaFile, config)
  }

  private val dots =
    <NamesWithDots xmlns="http://www.example.com/general"
                   at.="1"
                   at.at="1">
      <el.>suffix</el.>
      <el.el>middle</el.el>
    </NamesWithDots>

  private val hyphens =
    <NamesWithHyphens xmlns="http://www.example.com/general"
                      xmlns:xs="http://www.w3.org/2001/XMLSchema"
                      at-="1"
                      at-at="1">
      <el->suffix</el->
      <el-el>middle</el-el>
    </NamesWithHyphens>

  private val underscores =
    <NamesWithUnderscores xmlns="http://www.example.com/general"
                          xmlns:xs="http://www.w3.org/2001/XMLSchema"
                          _at="1"
                          at_="1"
                          at_at="1">
      <_el>prefix</_el>
      <el_>suffix</el_>
      <el_el>middle</el_el>
    </NamesWithUnderscores>

  private val doubleUnderscores =
    <NamesWithDoubleUnderscores xmlns="http://www.example.com/general"
                                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                                __at="1"
                                at__="1"
                                at__at="1">
      <__el>prefix</__el>
      <el__>suffix</el__>
      <el__el>middle</el__el>
    </NamesWithDoubleUnderscores>

  /** Checks that the given XML can be parsed into the given class name,
    * checks that there exists an attribute set to true for each of ''booleanAttributeNames'',
    * and checks that all ''elements'' are set to the expected values.
    */
  private def test(generated: Seq[File])(xml: Node, className: String, booleanAttributeNames: Seq[String], elements: Map[String, String]) = {
    val attributeChecks = booleanAttributeNames.map("x." + _)
    val elementChecks = elements.map { case (name, value) => s""" x.$name == "$value" """}
    val fullCheck = (attributeChecks ++ elementChecks).mkString(" && ")

    repl(generated)(s"""
      val obj = scalaxb.fromXML[$className](${Utility.trim(xml)})
      obj match {
        case x: $className if $fullCheck =>
          "success"
        case _ =>
          obj.toString
      }
    """, expectedResult = "success")
  }

  "The symbol encoding strategy" >> {
      var testedStrategies = Set.empty[SymbolEncoding.Strategy]

      def testSpecialSymbols(symbolEncodingStrategy: SymbolEncoding.Strategy,
                             symbolEncoder: Char => String) = {
        testedStrategies += symbolEncodingStrategy
        implicit lazy val generated: Seq[File] = generate(symbolEncodingStrategy)
        val Seq(encodedDot, encodedHyphen, trailingUnderscore) = Seq('.', '-', '_').map(symbolEncoder)

        if (symbolEncodingStrategy != SymbolEncoding.Discard) {
          "should leave underscores at the beginning and in the middle, and encode the one at the end" >> {
            implicit val generated = generate(symbolEncodingStrategy, schemaFile = doubleUnderscoresSchema)
            test(generated)(doubleUnderscores, "NamesWithDoubleUnderscores",
              Seq(s"at_${trailingUnderscore}", "__at", "at__at"),
              Map(s"el_${trailingUnderscore}" -> "suffix", "__el" -> "prefix", "el__el" -> "middle")
            )
          }
        }

        "should encode dots" >> test(generated)(dots, "NamesWithDots",
          Seq(s"at${encodedDot}", s"at${encodedDot}At"),
          Map(s"el${encodedDot}" -> "suffix", s"el${encodedDot}El" -> "middle")
        )

        "should encode hyphens" >> test(generated)(hyphens, "NamesWithHyphens",
          Seq(s"at${encodedHyphen}", s"at${encodedHyphen}At"),
          Map(s"el${encodedHyphen}" -> "suffix", s"el${encodedHyphen}El" -> "middle")
        )

        "should only encode the underscore at the end" >> test(generated)(underscores, "NamesWithUnderscores",
          Seq(s"at${trailingUnderscore}", "at_at", "_at"),
          Map(s"el${trailingUnderscore}" -> "suffix", "el_el" -> "middle", "_el" -> "prefix")
        )
      }

      "Discard" >>
        testSpecialSymbols(SymbolEncoding.Discard, symbolEncoder = _ => "")

      "UnicodePoint" >>
        testSpecialSymbols(SymbolEncoding.UnicodePoint, symbolEncoder = {
          case '.' => "U002e"
          case '-' => "U002d"
          case '_' => "U005f"
        })

      "SymbolName" >>
        testSpecialSymbols(SymbolEncoding.SymbolName, symbolEncoder = {
          case '.' => "Dot"
          case '-' => "Hyphen"
          case '_' => "Underscore"
        })

      "DecimalAscii" >>
        testSpecialSymbols(SymbolEncoding.DecimalAscii, symbolEncoder = {
          case '.' => "u46"
          case '-' => "u45"
          case '_' => "u95"
        })

      "Legacy151" >>
        testSpecialSymbols(SymbolEncoding.Legacy151, symbolEncoder = {
          case '.' => "u46"
          case '-' => "u45"
          case '_' => "u93"
        })

      "tests should have covered all strategies" >> { testedStrategies must containTheSameElementsAs(SymbolEncoding.values) }
  }
}
