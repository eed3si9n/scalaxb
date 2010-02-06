/* schema2src -- data binding tool
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: DtdHandler.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.dtd

import scala.xml._
import scala.xml.dtd._
import scala.xml.parsing._
import scala.collection.mutable.HashMap
// 2 do :  - handle modes of attributes ( #REQUIRED,  defaults for #IMPLIED )

/** Scala markup handler, reacts to events during declaration parsing */
abstract class DtdHandler extends DefaultMarkupHandler {
// DefaultMarkupHandler would override my methods??
/*
  def elem(pos: Int, pre: String, label: String, attrs: MetaData, scope:NamespaceBinding, args: NodeSeq) = NodeSeq.Empty;

  def procInstr(pos: Int, target: String, txt: String) = NodeSeq.Empty;

  def comment(pos: Int, comment: String ): NodeSeq = NodeSeq.Empty;
*/

  var elemMap: HashMap[String, MyElemDecl] = new HashMap[String, MyElemDecl]

  //                                        zzz    DTDHandler methods    zzz

  /** encountered element declaration */
  override def elemDecl(name: String, contentModel: String) {
    elemMap.get(name) match {
      case Some(decl) => // was added because of ATTLIST decl before
        elemMap.update(name, new MyElemDecl(decl.name, 
                                            contentModel, 
                                            decl.myAttribs))
      case None =>      
        elemMap.update(name, new MyElemDecl(name, 
                                            contentModel, 
                                            new HashMap[String,AttrDecl]()))
    }
  }
  
  override def attListDecl(elementName: String, adecls: List[AttrDecl]) {
    val attribs = elemMap.get(elementName) match {
      case None =>       
        val amap = new HashMap[String,AttrDecl]
        elemMap.update(elementName, 
                       new MyElemDecl( elementName, 
                       null:String, 
                       amap))
        amap 
      case Some(decl) => 
        decl.myAttribs
    }
    for (adecl <- adecls) 
      attribs.update(adecl.name, adecl)
  }
    
}
