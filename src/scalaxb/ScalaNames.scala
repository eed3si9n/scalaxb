/**
 * @author  e.e d3si9n
 */
 
package scalaxb

trait ScalaNames {

  def isKeyword(str: String) =
    str match {
      case "abstract"
      | "case"
      | "class"
      | "catch"
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
      | "import"
      | "new"
      | "null"
      | "object"
      | "override"
      | "package"
      | "private"
      | "protected"
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
