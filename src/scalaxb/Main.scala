/**
 * @author  e.e d3si9n
 */

package scalaxb

object Main {
  val module = scalaxb.xsd.Driver  
  
  def main(args: Array[String]) {
    try {
      module.config.args = args
      module.start()
    } 
    catch {
      case e: Exception =>
        e.printStackTrace
    }
  }
  
  def log(msg: String) {
    if (module.config.verbose) {
      println("["+msg+"]")
      Console.flush
    }
  }
}
