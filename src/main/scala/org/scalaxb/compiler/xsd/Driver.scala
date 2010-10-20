/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
 
package org.scalaxb.compiler.xsd

import org.scalaxb.compiler.{Module, Config}
import java.io.{File, Reader, PrintWriter}
import collection.mutable

trait PackageName {
  def packageName(schema: SchemaDecl, context: XsdContext): Option[String] =
    packageName(schema.targetNamespace, context)

  def packageName(decl: ComplexTypeDecl, context: XsdContext): Option[String] =
    packageName(decl.namespace, context)
  
  def packageName(decl: SimpleTypeDecl, context: XsdContext): Option[String] =
    packageName(decl.namespace, context)

  def packageName(group: AttributeGroupDecl, context: XsdContext): Option[String] =
    packageName(group.namespace, context)
      
  def packageName(namespace: Option[String], context: XsdContext): Option[String] =
    if (context.packageNames.contains(namespace)) context.packageNames(namespace)
    else if (context.packageNames.contains(None)) context.packageNames(None)
    else None
}

class Driver extends Module with PackageName { driver =>
  type Schema = SchemaDecl
  type Context = XsdContext
  
  override def buildContext = XsdContext()
    
  override def processContext(context: Context, cnfg: Config) {
    val processor = new ContextProcessor {
      val logger = driver
      val config = cnfg    
    }
    processor.processContext(context)
  }
  
  override def generate(xsd: Schema, context: Context, packageName: Option[String],
      cnfg: Config) = {
    log("xsd: generating package " + packageName)
    val generator = new GenSource(xsd, context, packageName) {
      val logger = driver
      val config = cnfg
    }
    generator.run
  }
  
  override def toImportable(in: Reader): Importable = new Importable {
    val reader = in
    val elem = scala.xml.XML.load(reader)
    val schemaLite = SchemaLite.fromXML(elem)
    val targetNamespace = schemaLite.targetNamespace
    val imports: Seq[String] = schemaLite.imports.collect {
      case ImportDecl(Some(namespace: String), _) => namespace
    }
    
    def toSchema(context: Context): Schema = {
      val schema = SchemaDecl.fromXML(elem, context)
      context.schemas += schema
      log("SchemaParser.parse: " + schema.toString())
      schema
    }
  }
}
