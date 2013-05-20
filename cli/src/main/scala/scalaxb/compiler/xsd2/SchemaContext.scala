/*
 * Copyright (c) 2011 e.e d3si9n
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

package scalaxb.compiler.xsd2

import scala.collection.mutable
import xmlschema._
import java.net.URI

case class SchemaContext(schemas: mutable.ListBuffer[ReferenceSchema] = mutable.ListBuffer(),
                         names: mutable.ListMap[Tagged[_], String] = mutable.ListMap(),
                         traitNames: mutable.ListMap[TaggedType[XComplexType], String] = mutable.ListMap(),
                         packageNames: mutable.ListMap[Option[String], Option[String]] = mutable.ListMap(),
                         baseToSubs: mutable.ListMap[TaggedType[XComplexType], List[TaggedType[XComplexType]]] = mutable.ListMap(),
                         prefixes: mutable.ListMap[URI, String] = mutable.ListMap())
