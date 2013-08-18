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
 
package scalaxb.compiler

trait ScalaNames {
  def isCommonlyUsedWord(s: String) = s match {
    case "All" 
    | "Any"
    | "AnyRef"
    | "Array"
    | "ArrayBuffer"
    | "ArraySeq"
    | "ArrayStack"
    | "Attributes"
    | "Base64Binary"
    | "BigDecimal"
    | "BigInt"
    | "BitSet"
    | "Boolean"
    | "Buffer"
    | "Byte"
    | "Char"
    | "Double"
    | "DoubleLinkedList"
    | "Elem"
    | "Failure"
    | "Float"
    | "Function"
    | "ImmutableMapAdaptor"
    | "ImmutableSetAdaptor"
    | "IndexedSeq"
    | "Input"
    | "Int"
    | "Iterable"
    | "HashMap"
    | "HashSet"
    | "HexBinary"
    | "Left"
    | "LinearSeq"
    | "LinkedHashMap"
    | "LinkedHashSet"
    | "LinkedList"
    | "List"
    | "ListBuffer"
    | "ListMap"
    | "ListSet"
    | "Long"
    | "Map"
    | "MutableList"
    | "None"
    | "NoSuccess"
    | "NumericRange"
    | "Object"
    | "ObservableBuffer"
    | "ObservableMap"
    | "ObservableSet"
    | "OnceParser"
    | "OpenHashMap"
    | "Option"
    | "Parser"
    | "ParserResult"
    | "PriorityQueue"
    | "Product"
    | "Queue"
    | "Range"
    | "Right"
    | "Seq"
    | "Set"
    | "Short"
    | "SortedSet"
    | "SortedMap"
    | "Stack"
    | "Stream"
    | "String"
    | "StringBuilder"
    | "Success"
    | "SynchronizedBuffer"
    | "SynchronizedMap"
    | "SynchronizedSet"
    | "SynchronizedStack"
    | "SynchronizedPriorityQueue"
    | "SynchronizedQueue"
    | "Traversable"
    | "TreeMap"
    | "TreeSet"
    | "Unit"
    | "WeakHashMap"
     => true
    case _ => false
  }
  
  def isSpecialAttributeWord(str: String) =
    str match {
      case "value" => true
      case _ => false
    }
  
  def isKeyword(str: String) =
    str match {
      case "abstract"
      | "case"
      | "catch"
      | "class"
      | "def"
      | "do"
      | "else"
      | "extends"
      | "false"
      | "final"
      | "finally"
      | "for"
      | "forSome"
      | "if"
      | "implicit"
      | "import"
      | "lazy"
      | "match"
      | "new"
      | "null"
      | "object"
      | "override"
      | "package"
      | "private"
      | "protected"
      | "requires"
      | "return"
      | "sealed"
      | "super"
      | "this"
      | "throw"
      | "trait"
      | "true"
      | "try"
      | "type"
      | "val"
      | "var"
      | "with"
      | "while"
      | "yield" => true

      case _ => false
    }
    /* // these cannot appear as XML names
  case "." =>
  case "_" =>
  case ":" =>
  case "=" =>
  case "=>" =>
  case "<-" =>
  case "<:" =>
  case ">:" =>
  case "<%" =>
  case "#" =>
  case "@" =>
    */

}
