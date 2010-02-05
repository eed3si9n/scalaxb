/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: GenSource.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.dtd

import scala.collection.Map
import scala.collection.mutable.HashMap

import scala.xml._
import scala.xml.dtd.AttrDecl

/** transforms a set of DTD declaraion to a scala source file.
 *
 *  From the DTD contained in elemMap, we generate classes in source form
 *  which will (shallow-)validate inside the constructors.
 *
 *  Using Scala's automata library, the most convenient and efficient option
 *  is to compile the content model to a dfa and generate a source that
 *  will re-construct this dfa
 */
class GenSource(conf: Driver.DtdConfig, elemMap: Map[String, MyElemDecl]) extends ScalaNames {

  import conf.{outfile => fOut, objName => objectName, namespace}
  
  /*
   def tmpSource() = {
   val is = ClassLoader.getSystemResourceAsStream(tmplFile);
   val s = new scala.io.Source {
   def reset = tmpSource();
   }
   }
   */
  
  /** runs translation. */
  def run {
    def myprint(n: Node) = n match {
      case Text(s)          => fOut.print(s)
      case EntityRef("amp") => fOut.print('&')
      case a:Atom[_]        => fOut.print(a.data)
      case _                => Main.log("error in dtd:run: encountered "+n.getClass())
    }
    fOut println "object "+objectName+" {";
    for (val n <- makeBinder.child) 
      myprint(n)

    for (val decl <- elemMap.values) {
      for (val n <- makeClass(decl.name, decl.cmString, decl.theAttribs.values.toList).child ) {
	//Console.println("made "+n.toString());
	myprint(n)
      }
    }
    fOut println "}"
  }
  
  private def warning_attrib(name: String, tpe: String) {
    System.err.print("[ignoring attribute \"")
    System.err.print(name)
    System.err.print("\" of type \"")
    System.err.print(tpe)
    System.err.print("\"]")
  }
  

  //
  // cooking raw names
  //
  
  /* replace dash, colons with underscore, keywords with appended $ */
  private def cooked(ckd: StringBuilder, raw: String, off: Int): String = {
    for (val i <- List.range(off, raw.length()))
      raw.charAt(i) match {
        case '-' =>
          ckd.append( '_' )
        case ':' =>
          ckd.append( '_' )
        case x =>
          ckd.append(x)
      }
    
    if (isKeyword(raw))
      ckd.append('$')

    ckd.toString()
  }
  
  // type       -> type$
  // http-equiv -> http_equiv
  
  private def cooked(raw: String): String = cooked(new StringBuilder(), raw, 0)
  
  // type       -> Type$
  // http-equiv -> Http_equiv
  
  private def cookedCap(raw: String): String = {
    val ckd = new StringBuilder()
    ckd.append(Character.toUpperCase(raw.charAt(0)))
    cooked(ckd, raw, 1)
  }
  

  def makeBinder: scala.xml.Node = {

    def makeDecl(name: String) = 
      "scala.xml.dtd.ElemDecl(\""+name+"\", "+name+".contentModel)"

      <moresource>
/* */   object binder extends scala.xml.factory.Binder(true) {{
/* */     def reportValidationError(pos: int, msg: String) = {{}} // empty
/* */     override def elem(pos: int, pre: String, label: String,
                            attrs: scala.xml.MetaData,
                            pscope: scala.xml.NamespaceBinding,
                            nodes: scala.xml.NodeSeq): scala.xml.NodeSeq = label match {{
        { val sb = new StringBuilder()
          val it = elemMap.values
          while (it.hasNext) {
            val decl = it.next
            sb.append("case \"").append(decl.name).append("\" => ").append(decl.name).append("(attrs, nodes:_*)").append('\n')
          }
          sb.toString()
        }
/* */    }} // def elem(...)
/* */ this.decls = List(
/* */ { {val it = elemMap.values
/* */    val sb = new StringBuilder()
/* */    sb.append(makeDecl(it.next.name))
/* */    while (it.hasNext) {
/* */      sb.append(',')
/* */      sb.append(makeDecl(it.next.name))
/* */    }
/* */    sb.toString()
/* */ }});}}
</moresource>;
  }

  /* @todo preserve spaces?!? */
  def makeClass(elemName: String, cmString: String, attList: List[AttrDecl]) = {
    import scala.xml.dtd._
    def _namespace() = if (namespace != null) "\""+namespace+"\"" else "null"
    def _setNamespace() = if (namespace != null) "tmp.setNamespace("+namespace+");" else ""

    def _ddecl2string(ddecl: DefaultDecl): String = ddecl match {
      case DEFAULT(true, vlue)  => "scala.xml.dtd.DEFAULT(true, \""+vlue+"\"))"
      case DEFAULT(false, null) => "scala.xml.dtd.DEFAULT(false, null)"
      case DEFAULT(false, vlue) => "scala.xml.dtd.DEFAULT(false, \""+vlue+"\"))"
      case REQUIRED             => "scala.xml.dtd.REQUIRED"
      case IMPLIED              => "scala.xml.dtd.IMPLIED"
      case _ => error("doesn't make sense:" + ddecl)
    }
    def _attList() = attList.foldLeft ("List(") { (x,y) => x + (y match {
      case AttrDecl(key,tpe, ddecl) =>
        "scala.xml.dtd.AttrDecl(\""+key+"\",\""+tpe+"\","+ _ddecl2string(ddecl)+")"
    })} + ")";

    <source>
&#x0a;    object {" "+elemName} {{
      val contentModel = scala.xml.dtd.ContentModel.parse({"\""+cmString+"\""})
      val tmp = new scala.xml.dtd.ElementValidator()
      tmp.setContentModel(contentModel)
      { _setNamespace() }
      tmp.setMetaData({ _attList() })
      def validate(_attributes: scala.xml.MetaData, child: Seq[scala.xml.Node]) =
      if(!(tmp.check(child)&amp;&amp;tmp.check(_attributes)))
        throw new scala.xml.dtd.ValidationException(tmp.exc.toString())
    }}
&#x0a;
&#x0a;
&#x0a;    /** {elemName} &#x0a; ({cmString}) &#x0a; */
&#x0a;    case class {" "+elemName}(_attributes: scala.xml.MetaData, child: scala.xml.Node*)
	 extends scala.xml.Node {{
&#x0a;
           def this(s: String) = this(scala.xml.Null,scala.xml.Text(s))
&#x0a;
	   
	   final def label = "{elemName}"
	   final override def namespace = { _namespace() }
	   final override def attributes = _attributes
	   {elemName}.validate(_attributes, child)

   override def text = {{
     val sb = new StringBuilder()
     val it = child.elements
     while (it.hasNext) {{
       sb.append(it.next.text)
     }}
     sb.toString()
   }}

}}; /* &#x0a; */
def {" "+elemName}(s: String) = new {" "+elemName}(s);
&#x0a;
     </source>
         } 
}
