

object parseSchema {
  
  import scala.collection.immutable.{ Map, ListMap };
  import scala.io.Source ;
  import scala.xml._ ;
  import scala.xml.xsd._ ;
  import scala.xml.parsing.ConstructingParser;

  /** - names and lifts anonymous types
   *  - desugarizes 
   *    complexContent(x) => complexContent(restriction(@base="xs:anyType"), x)
   *      if x not in (extension|restriction)
   *  - removes "annotation"
   */
  class Lifter {
    var ii = 0;
    def fresh(name:String) = { 
      val res = name + "$"+ii.toString();
      ii = ii + 1;
      res
    }
    var moreDecls: List[Node] = Nil;

    /** */
    def desugarize(ct: Node) = 
      if(isComplexType(ct)) ct.child(0).label match {
        case "simpleContent" | "complexContent" => ct // ok
        case _ => 
          val xs = xsPrefix(ct.scope);
          val md = new UnprefixedAttribute("base", xs+":AnyType", Null);
          Elem(xs,"complexContent", Null, ct.scope,
               Elem(xs,"restriction", md, ct.scope, ct.child:_*))
      } else ct;
    
    def transform(x: Seq[Node]): Seq[Node] = 
      x.toList filter { z => !isAnnotation(z) } map transform;

    def transform(x: Node): Node = x match {

      case _ if isElem(x) && ((x \ "@type") concat (x \ "@ref")).length == 0 =>

        // anonymous
        val z       = desugarize(x.child(0)); // complexType/simpleType
        val nchild  = transform(z.child);
        val tpeName = fresh(x.attribute("name").toString());
        val md      = new UnprefixedAttribute("name", tpeName, z.attributes);
        val decl    = Elem( z.prefix, z.label, md, z.scope, nchild:_* );
        moreDecls = decl :: moreDecls;

        val md2 = new UnprefixedAttribute("name", tpeName, Null);
        Elem( z.prefix, z.label, md2, z.scope, Nil:_*);

      case Elem(pre,lab,md,scp,child@_*) =>
        val z = Elem(pre,lab,md,scp,transform(child):_*);
        desugarize(z)

      case _ => x
    }
    def lift(x:Node) = {
      val y = transform(x); 
      val nchild = moreDecls concat y.child;
      Elem(y.prefix, y.label, y.attributes, y.scope, nchild:_*);
    }
  }

  final val xsdns = "http://www.w3.org/2001/XMLSchema";

  final def xsPrefix(scp:NamespaceBinding): String = 
    if(scp==TopScope)
      error("cannot find xsd namespace ?!"); // cannot happen
    else if (scp.uri == xsdns)
      scp.prefix;
    else 
      xsPrefix(scp.parent);
  

  def isAnnotation(x:Node)  = x.label == "annotation"  && x.namespace == xsdns;
  def isElem(x:Node)        = x.label == "element"     && x.namespace == xsdns;
  def isComplexType(x:Node) = x.label == "complexType" && x.namespace == xsdns;
  def isSimpleType(x:Node)  = x.label == "simpleType"  && x.namespace == xsdns;
  def isExtension(x:Node)   = x.label == "extension"   && x.namespace == xsdns;
  def isRestriction(x:Node) = x.label == "restriction" && x.namespace == xsdns;

  var elems:   Map[Pair[String,String],ElemDecl] = _;
  var symbols: Map[Pair[String,String],XsTypeSymbol]   = _;
  var targetNamespace: String = null;

  def initElems = {
    elems = ListMap.Empty[Pair[String,String],ElemDecl];
  }

  def initSymbols = {
    symbols = 
      ListMap.Empty[Pair[String,String],XsTypeSymbol]
      .update(Pair(xsdns, "boolean"), xsBoolean)
      .update(Pair(xsdns, "double"),  xsDouble)
      .update(Pair(xsdns, "float"),   xsFloat)
      .update(Pair(xsdns, "int"),     xsInt)
      .update(Pair(xsdns, "long"),    xsLong)
      .update(Pair(xsdns, "short"),   xsShort)
      .update(Pair(xsdns, "string"),  xsString);
  }

  def symbol(s1:String, s2:String) = symbols.get(Pair(s1,s2));

  /** returns null if tpe is not bound by a builtin */
  def lookup(scp:NamespaceBinding, tpe:String): Option[XsTypeSymbol] = {
    tpe.indexOf(':') match {
      case -1 =>
        val ns = scp.getURI(null);
        symbol(ns,tpe);
      
      case i => 
        val pr = tpe.substring(0,i);
        val ns = scp.getURI(pr);
        val id = tpe.substring(i+1,tpe.length());
        //Console.println("[lookup] pr="+pr);
        //Console.println("[lookup] ns="+ns);
        //Console.println("[lookup] id="+id);
        symbol(ns, id)
    }
  }

  def main(args:Array[String]): Unit = {

    if(args.length != 1)
      return {};

    val p = new ConstructingParser(Source.fromFile(args(0)), false);
    p.nextch; //init

    Console.println("init symbol, elem table");
    
    initSymbols;
    initElems;
    
    Console.println("parse schema");

    var xsd = p.element(TopScope)(0);


    Console.print("set target namespace to: ");
    targetNamespace = xsd.attribute("targetNamespace").toString();
    Console.println(targetNamespace);

    Console.println("lift anonymous types");
    
    xsd = new Lifter().lift(xsd);
    Console.println(new PrettyPrinter(80,3).format(xsd));

    Console.println("make type symbols");

    Console.println("--complex types:");
    for(val c <- xsd \ "complexType") {
      val name = c.attribute("name").toString();
      val csym = new ComplexTypeSymbol(name);

      Console.println(" "+name);
      symbols = symbols.update(Pair(targetNamespace, name), csym);
      
    }

    Console.println("--simple types:");
    for(val c <- xsd \ "simpleType") {
      val name = c.attribute("name").toString();
      val csym = new SimpleTypeSymbol(name);

      Console.println(" "+name);
      symbols = symbols.update(Pair(targetNamespace, name), csym);
      
    }


    Console.println("--top-level elements:");
    for(val c <- xsd \ "element") { // these all have a name
      val name   = c.attribute("name").toString();
      val tpeRef = c.attribute("type").toString();
      // lookup type
      lookup(c.scope, tpeRef) match {
        case None => 
          Console.println("error: type \""+tpeRef+"\" not found");
        case Some(tpe) =>
          val decl   = ElemDecl(name, tpe); 
          Console.println(" "+decl);
          elems = elems.update(Pair(targetNamespace,name), decl);
      }
    }

    /*
    Console.println("--all elements:");
    Console.println((xsd \\ "element").length);
    for(val c <- xsd \\ "element") {

      val ref: String = c.attribute("ref");
      if (ref!=null) { // reference .... is an element reference anyway
      } else {
        val tpe: String = c.attribute("type"); // anonymous type
        if (tpe!=null) {
          val ed = ElemDecl(c.attribute("name"), c.attribute("tpe"));
          Console.println(ed);
        } else {
        // anonymous type
      }

    }
    */

    Console.println("end (for now)");
    
    Console.println("symbols:");
    for(val Pair(k,v) <- symbols.elements) {
    Console.println(" "+k+" -> "+v);
    }
    Console.println("elems:"+elems.toString());
    
  }
}
