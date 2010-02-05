/* schema2src -- data binding tool
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: GenSource.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.xsd

import scala.xml.xsd.{ ElemDecl, TypeDecl, XsTypeSymbol }
import scala.collection.Map

/** transforms a set of XSD declaraions to a scala source file.
 *
 *  From the XSD contained in elemMap, we generate classes in source form
 *  which will (shallow-)validate inside the constructors.
 *
 *  Using Scala's automata library, the most convenient and efficient option
 *  is to compile the content model to a dfa and generate a source that
 *  will re-construct this dfa
 */
class GenSource(conf: Driver.XsdConfig, elemMap: (Map[String,ElemDecl], Map[String,TypeDecl])) extends ScalaNames {

  import conf.{outfile => fOut, objName => objectName, namespace}

  def tpeSymName(name: String): String = name+"_tpesym"

  /** template for binder, meaning the object that will play the handler
   *  role for when parsing conforming xml 
   *  and bind the incoming stuff to some scala class
   */

  def makeBinder: scala.xml.Node = {

    def makeSymRef(name: String): String = name match {
      case "boolean" => "scala.xml.xsd.xsBoolean"
      case "double"  => "scala.xml.xsd.xsDouble"
      case "float"   => "scala.xml.xsd.xsFloat"
      case "int"     => "scala.xml.xsd.xsInt"
      case "long"    => "scala.xml.xsd.xsLong"
      case "string"  => "scala.xml.xsd.xsString"
      case "date"    => "scala.xml.xsd.xsDate"
      case _ => name+"__Type"
    }

    def makeSym(decl:TypeDecl) = decl match {
      case SimpleTypeDecl(name) => 
        val n = name+"__Type"
        "new scala.xml.xsd.SimpleTypeSymbol("+n+")"
      case ComplexTypeDecl(name, null, cmodel) =>
        val n = name+"__Type"
        "new scala.xml.xsd.ComplexTypeSymbol("+n+",null,"+????????+")"
      case ComplexTypeDecl(name, Extends(sUper), cmodel) =>
        val n = name+"__Type"
        "new scala.xml.xsd.ComplexTypeSymbol("+n+",scala.xml.xsd.Extends"+makeSymRef(derivedFrom)+","+????????+")"
      case ComplexTypeDecl(name, Restricts(sUper), cmodel) =>
        "new scala.xml.xsd.ComplexTypeSymbol("+n+",scala.xml.xsd.Extends"+makeSymRef(derivedFrom)+","+????????+")"
    }

    def makeDecl(decl:ElemDecl) = 
      "scala.xml.xsd.ElemDecl(\""+decl.name+"\", "+tpeSymRef(decl.name)+")"
    
    return <moresource>
/* */    object binder extends scala.xml.factory.Binder(true) {{
/* */      def reportValidationError(pos:int, msg:String) = {{}; // empty
/* */      override def elem(pos: int, pre: String, label: String, attrs: scala.xml.MetaData, pscope: scala.xml.NamespaceBinding, nodes: scala.xml.NodeSeq): scala.xml.NodeSeq = label match {{
/* */
      { {val sb = new StringBuffer(); 
         val it = elemMap.values;
         while(it.hasNext) {
           val decl = it.next;
           sb.append("case \"").append(decl.name).append("\" => ").append(decl.name).append("(attrs, nodes:_*)"); 
         }
         sb.toString()
       }
     }
/* */      }; // def elem(...)
/* */ // make type symbols
/* */ this.syms = List(
        { (elemMap._2.values map makeSym).mkString("",",","") }
/* */ );
/* */ // make declarations
/* */ this.decls = List(
       { (elemMap._1.values mak makeDecl).mkString("",",","") }
/* */ }});}
</moresource>;
    

  }
  
  def run {
    def myprint(n: Node) = n match {
      case Text(s)          => fOut.print(s)
      case EntityRef("amp") => fOut.print('&')
      case _                => Main.log("error in dtd:run: encountered "+n.getClass())
    }

    fOut println "object "+objectName+" {";
    
    fOut println "}"
  }
}
