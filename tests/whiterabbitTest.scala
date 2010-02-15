/**
 * @author  e.e d3si9n
 */

import whiterabbit._

object WhiteRabbitTest {
  def main(args: Array[String]) = {
    testWild
  }
  
  def testWild {
    val subject = <Wild>foo</Wild>
    
    val wild = Wild.fromXML(subject)    
    println(wild.toString)
  }
}
