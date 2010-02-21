/**
 * @author  e.e d3si9n
 */

import whiterabbit._

object WhiteRabbitTest {
  def main(args: Array[String]) = {
    testWild
    testRiskOption
    testRisk
  }
  
  def testWild {
    val subject = <ProcessPtnWild>
      <Wild>foo</Wild>
    </ProcessPtnWild>
    val wild = Wild.fromXML(subject)
    wild match {
      case Wild("foo") =>
      case _ => throw new Exception("match failed: " + wild.toString)
    } 
    println(wild.toString)
  }
  
  def testRiskOption {
    val subject = <Agent/>    
    val riskOption = RiskOption.fromXML(subject)
    riskOption match {
      case Agent() =>
      case _ => throw new Exception("match failed: " + riskOption.toString)
    }
    
    println(riskOption.toString)
  }
  
  def testRisk {
    val subject = <Risk>
      <Agent/>
      <Manifested/>
    </Risk>
        
    val risk = Risk.fromXML(subject)
    risk match {
      case Risk(Agent(), Manifested(List())) =>
      case _ => throw new Exception("match failed: " + risk.toString)
    }
    
    println(risk.toString)
  }
}
