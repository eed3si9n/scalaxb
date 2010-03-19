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

import org.scalaxb.compiler.{Module}
import java.io.{File}
import collection.mutable

object Driver extends Module {  
  type Schema = SchemaDecl
  type Context = XsdContext

  val schemaLites = mutable.ListMap.empty[File, SchemaLite]
  val schemaFiles = mutable.ListMap.empty[String, File]
  val processor = new ContextProcessor(this)
  
  override def buildContext = XsdContext()
  
  def parse(input: File, context: Context): Schema = {
    log("xsd: parsing " + input)
    val elem = scala.xml.XML.loadFile(input)
    val schema = SchemaDecl.fromXML(elem, context)
    context.schemas += schema

    log("SchemaParser.parse: " + schema.toString())
    schema
  }
  
  override def processContext(context: Context,
      packageNames: collection.Map[String, Option[String]]) {
    processor.processContext(context, packageNames)
  }
  
  override def packageName(schema: Schema, context: Context): Option[String] =
    processor.packageName(schema, context)
  
  def generate(xsd: Schema, context: Context, output: File,
      packageName: Option[String], firstOfPackage: Boolean) = {
    val out = new java.io.PrintWriter(new java.io.FileWriter(output))
    log("xsd: generating ...")
    if (!context.typeNames.contains(packageName))
      context.typeNames += (packageName -> mutable.ListMap.
                            empty[ComplexTypeDecl, String])
    
    new GenSource(xsd, context, out, packageName, firstOfPackage, this) run;
    out.flush()
    out.close()
    println("generated " + output)
    output
  }
  
  override def sortByDependency(files: Seq[File]): Seq[File] = {
    schemaLites.clear
    schemaFiles.clear
    for (file <- files) {
      val schemaLite = preparse(file)
      schemaLites += (file -> schemaLite)
      if (schemaLite.targetNamespace != null)
         schemaFiles += (schemaLite.targetNamespace -> file)
    }

    val XML_URI = "http://www.w3.org/XML/1998/namespace"
    val xmlxsd = new File("xml.xsd")
    schemaFiles += (XML_URI -> xmlxsd)
    
    val unsorted = mutable.ListBuffer.empty[File] ++= files
    val sorted = mutable.ListBuffer.empty[File]
    val upperlimit = unsorted.size * unsorted.size

    sorted += xmlxsd
    def containsAll(schemaLite: SchemaLite) = schemaLite.imports.partialMap {
      case ImportDecl(Some(namespace: String), _) => schemaFiles(namespace)
    }.forall(file => sorted.contains(file))
    
    for (i <- 0 to upperlimit) {
      if (unsorted.size > 0) {
        val file = unsorted(i % unsorted.size)
        val schemaLite = schemaLites(file)
        if ((schemaLite.imports.size == 0) ||
            containsAll(schemaLite)) {
          unsorted -= file
          sorted += file
        } // if
      } // if
    }

    sorted -= xmlxsd

    if (unsorted.size > 0)
      error("Circular import: " + unsorted.toList)
    sorted
  }

  def preparse(input: File): SchemaLite = {
    log("xsd: pre=parsing " + input)
    val elem = scala.xml.XML.loadFile(input)
    val schema = SchemaLite.fromXML(elem)
    schema
  }
}
