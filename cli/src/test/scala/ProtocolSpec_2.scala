package scalaxb.specs

import org.specs2._

object ProtocolSpec_2 extends Specification { def is =                        s2"""             
  this is a specification to check the generated protocol source
  
  xs:anyType should
    generate combinator parser                                                $anytype1
  
  substitution group should
    generate combinator parser                                                $sub1"""

  import Example._
  lazy val module = new scalaxb.compiler.xsd2.Driver
  // scalaxb.compiler.Log.configureLogger(true)
  val anyTypeProtocolSource = module.processNode(anyTypeXML, "example")(1)

  def anytype1 = {
    println(anyTypeProtocolSource)
    anyTypeProtocolSource.lines.toList must contain(
      """      scalaxb.fromXML[scalaxb.DataRecord[Option[Any]]](x, scalaxb.ElemName(node) :: stack)""")
    }

  val subgroupProtocolSource = module.processNode(subXML, "example")(1)
  def sub1 = {
    println(subgroupProtocolSource)
    (subgroupProtocolSource must contain(
      """phrase((((any(_ => true) ^^ (scalaxb.fromXML[scalaxb.DataRecord[Any]](_, scalaxb.ElemName(node) :: stack)))) | ((scalaxb.ElemName(Some("http://www.example.com/general"), "SubGroupMember")) ^^ ({ x =>
    scalaxb.DataRecord(x.namespace, Some(x.name), scalaxb.fromXML[example.MilkType](x, scalaxb.ElemName(node) :: stack))
  })) | ((scalaxb.ElemName(Some("http://www.example.com/general"), "SubGroupMember2")) ^^ ({ x =>
    scalaxb.DataRecord(x.namespace, Some(x.name), scalaxb.fromXML[Int](x, scalaxb.ElemName(node) :: stack))
  })) | ((scalaxb.ElemName(Some("http://www.example.com/general"), "SubGroupMember")) ^^ ({ x =>
    scalaxb.DataRecord(x.namespace, Some(x.name), scalaxb.fromXML[example.MilkType](x, scalaxb.ElemName(node) :: stack))
  })) | ((scalaxb.ElemName(Some("http://www.example.com/general"), "SubGroupMember2")) ^^ ({ x =>
    scalaxb.DataRecord(x.namespace, Some(x.name), scalaxb.fromXML[Int](x, scalaxb.ElemName(node) :: stack))
  }))) ^^ {
    case p1 => example.SubstitutionGroupTest(p1)
  })"""
    ))
  }
}
