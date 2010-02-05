/* schema2src -- data binding tool
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Burak Emir
 */
// $Id: MyElemDecl.scala 13960 2008-02-12 17:49:29Z michelou $

package schema2src.dtd

import scala.collection.mutable.HashMap
import scala.xml.dtd.{AttrDecl, ContentModel, ElemDecl}

class MyElemDecl(override val name: String, val cmString: String,
                 val theAttribs: HashMap[String, AttrDecl])
extends ElemDecl(name, ContentModel.parse(cmString)) {
  def myAttribs = theAttribs
}

