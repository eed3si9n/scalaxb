import java.io.{File}

class WithBigDecimalTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/withbigdecimal.xsd")
  lazy val generated = module.process(inFile, "bigdecimal", tmp)
  
  "withbigdecimal.scala must properly format BigDecimal attributes" in {
    (List("import scalaxb._",
      "import bigdecimal._",
      """val document = <foo xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  attribute1="0.0000000023"
        |  optionalAttribute="0.00000000000000002" />""".stripMargin,
      """toXML[Foo](fromXML[Foo](document),
          None, Some("foo"), scalaxb.toScope(
            Some("xs") -> "http://www.w3.org/2001/XMLSchema",
            Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance"
          )).toString"""
     ),
     generated) must evaluateTo(
      """<foo optionalAttribute="0.00000000000000002" """ +
        """attribute1="0.0000000023" """ +
        """xmlns:xs="http://www.w3.org/2001/XMLSchema" """ +
        """xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>""",
     outdir = "./tmp")

    (List("import scalaxb._",
      "import bigdecimal._",
      """val document = <bar xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        |      0.42
        |  </bar>""".stripMargin,
      """toXML[Bar](fromXML[Bar](document),
          None, Some("bar"), scalaxb.toScope(
            Some("xs") -> "http://www.w3.org/2001/XMLSchema",
            Some("xsi") -> "http://www.w3.org/2001/XMLSchema-instance"
          )).toString"""
     ),
     generated) must evaluateTo(
      """<bar """ +
        """xmlns:xs="http://www.w3.org/2001/XMLSchema" """ +
        """xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">0.42</bar>""",
     outdir = "./tmp")

  }
}
