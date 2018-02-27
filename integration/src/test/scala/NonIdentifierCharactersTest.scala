import java.io.File

import scalaxb.compiler.Config
import scalaxb.compiler.ConfigEntry._

import scala.xml._

object NonIdentifierCharactersTest extends TestBase {
  private val schema = resource("non_identifier_characters.xsd")
  private val doubleUnderscoresSchema = resource("double_underscores.xsd")

  private def generate(discardNonIdentifierCharacters: Boolean = false,
                       replaceSpecialSymbolsWithNames: Boolean = false,
                       schemaFile: File = schema) = {
    var config = Config.default.update(Outdir(tmp))
    if (discardNonIdentifierCharacters)
      config = config.update(SymbolEncoding.Discard)
    if (replaceSpecialSymbolsWithNames)
      config = config.update(SymbolEncoding.SymbolName)
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

  "DiscardNonIdentifierCharacters" >> {
    "when set" >> {
      lazy val generated = generate(discardNonIdentifierCharacters = true)

      "should remove dots" >> {
        test(generated)(dots, "NamesWithDots", Seq("at", "atAt"), Map("el" -> "suffix", "elEl" -> "middle"))
      }

      "should remove hyphens" >> {
        test(generated)(hyphens, "NamesWithHyphens", Seq("at", "atAt"), Map("el" -> "suffix", "elEl" -> "middle"))
      }

      "should remove the underscore at the end" >> {
        test(generated)(underscores, "NamesWithUnderscores", Seq("at"), Map("el" -> "suffix"))
      }

      "should leave underscores at the beginning and in the middle" >> {
        test(generated)(underscores, "NamesWithUnderscores", Seq("_at", "at_at"), Map("_el" -> "prefix", "el_el" -> "middle"))
      }
    }

    "when unset" >> {

      def testSpecialSymbols(replaceSpecialSymbolsWithNames: Boolean, symbolEncoder: Char => String) = {
        implicit lazy val generated: Seq[File] = generate(replaceSpecialSymbolsWithNames = replaceSpecialSymbolsWithNames)
        val Seq(encodedDot, encodedHyphen, trailingUnderscore) = Seq('.', '-', '_').map(symbolEncoder)

        "should replace dots with their name" >> test(generated)(dots, "NamesWithDots",
          Seq(s"at${encodedDot}", s"at${encodedDot}At"),
          Map(s"el${encodedDot}" -> "suffix", s"el${encodedDot}El" -> "middle")
        )

        "should replace hyphens with their name" >> test(generated)(hyphens, "NamesWithHyphens",
          Seq(s"at${encodedHyphen}", s"at${encodedHyphen}At"),
          Map(s"el${encodedHyphen}" -> "suffix", s"el${encodedHyphen}El" -> "middle")
        )

        "should leave underscores and encode the one at the end" >> {
          implicit val generated = generate(replaceSpecialSymbolsWithNames = replaceSpecialSymbolsWithNames, schemaFile = doubleUnderscoresSchema)
          test(generated)(doubleUnderscores, "NamesWithDoubleUnderscores",
            Seq(s"at_${trailingUnderscore}", "__at", "at__at"),
            Map(s"el_${trailingUnderscore}" -> "suffix", "__el" -> "prefix", "el__el" -> "middle")
          )
        }
      }

      "when ReplaceSpecialSymbolsWithNames is disabled" >>
        testSpecialSymbols(replaceSpecialSymbolsWithNames = false, symbolEncoder = {
          case '.' => "U002e"
          case '-' => "U002d"
          case '_' => "U005f"
        })

      "when ReplaceSpecialSymbolsWithNames is enabled" >>
        testSpecialSymbols(replaceSpecialSymbolsWithNames = true, symbolEncoder = {
          case '.' => "Dot"
          case '-' => "Hyphen"
          case '_' => "Underscore"
        })
    }
  }
}
