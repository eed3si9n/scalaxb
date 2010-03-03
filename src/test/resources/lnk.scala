// $Id: lnk.scala,v 1.9 2004/07/08 17:06:29 buraq Exp $

import scala.xml._;
import scala.xml.dtd.{ ElemDecl };
import scala.xml.factory.Binder;
import scala.xml.parsing.{ MarkupHandler, ValidatingMarkupHandler };
import lnk._;

/*
object LnkBinder extends Binder(true) {

  def reportValidationError(pos:Int, msg:String) = {}

    override def elem(pos: Int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq = label match {
   case "link"        => link(attrs, nodes:_*)
   case "lnkDB"       => lnkDB(attrs, nodes:_*)
   case "linkgroup"   => linkgroup(attrs, nodes:_*)
   case "description" => description(attrs, nodes:_*)
   case "name"        => name(attrs, nodes:_*)
   case "groupname"   => groupname(attrs, nodes:_*)
  }
  
}
*/
object Test {

  // 3 ways to construct your data
  def main(args:Array[String]) = {
    
    // construct data using original Element name
    //val b: Node = dtd._factory.get("link").match { case Some(x) => x(Nil,null)}; // safe
    // !!! System.out.println(b.toXML);

    val x = name(Null, Text("hello-link"));

    // construct data using constructor (valid)
    val c = link(
      new UnprefixedAttribute("target","http://www.scala.org", Null),
      name(Null, Text("hello-link"))
    );


    Console.print("validator throws exception?");
    try {
      val c2 = name( Null, link( Null ));
      Console.println("eh ?");
    } catch {
      case scala.xml.dtd.ValidationException(msg) => {
        Console.println("ok.");
      }
      case z => // ignore
        Console.println("whut??? "+z.getClass);      
    }
    
    //c.getAttribs.update("target", "http://www.scala.org");
    System.out.println( c );

    // construct data using loading from a file
    //val lnkDB = load(args(0)); /* TODO */
    //System.out.println( lnkDB );

    val z = lnk.binder.validate {
      <link target="foo.com"><name>Hello</name></link>
    };

    z match {
      case link(_,name(_,Text("Hello"))) => 
        Console.println("it works! ");
    }
  }

}
