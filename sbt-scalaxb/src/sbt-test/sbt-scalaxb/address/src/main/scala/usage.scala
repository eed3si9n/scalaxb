import ipo._

object Usage extends App {
  val shipTo = scalaxb.fromXML[ipo.Address](<shipTo>
      <name>foo</name>
      <street>1537 Paper Street</street>
      <city>Wilmington</city>
    </shipTo>)
  println(shipTo.toString)   
}
