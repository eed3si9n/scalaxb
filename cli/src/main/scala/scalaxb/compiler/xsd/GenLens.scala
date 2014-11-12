package scalaxb.compiler.xsd

import scalaxb.compiler.Config

trait GenLens extends ContextProcessor{
  def buildImport: String
  def buildDefLens(className: String, param: Params#Param): String
  def buildDefComposeLens(className: String, param: Params#Param): String
  def buildObjectLens(localName: String, defLenses: String, defComposeLenses: String): String
}

/**
 * this class aims to generate lens using scalaz. it is inspired from Gerolf Seitz work: Lensed
 *
 *  case class Person(name: String, address: Address)
 *  case class Address(city: String)
 *
 *  Generated Code =================================
 *
 *  object Person {
 *    def name: Lens[Person, String] =  Lens.lensu((p: Person, name :String) => p.copy(name = name), _.name)
 *    def address: Lens[Person, Address] = Lens.lensu((p: Person, address : Address) => p.copy(address = address), _.address)
 *
 *    class PersonW[A](l: Lens[A, Person]) {
 *      def name: Lens[A, String] = l andThen Person.name
 *      def address: Lens[A, Address] = l andThen Person.address
 *    }
 *
 *    implicit def lens2personW[A](l: Lens[A, Person]): PersonW[A] = new PersonW(l)
 *  }
 *
 *  object Address {
 *    def city: Lens[Address, String] = Lens.lensu((address: Address, city : String) => address.copy(city = city), _.city)
 *
 *    class AddressW[A](l: Lens[A, Address]) {
 *      def city: Lens[A, String] = l andThen Address.city
 *    }
 *
 *  implicit def lens2addressW[A](l: Lens[A, Address]): AddressW[A] = new AddressW(l)
 *  }
 * @param config
 */

class GenScalazLens (val config: Config) extends GenLens{

  def buildImport: String  = {
    "import scalaz._"
  }

  def buildDefLens(className : String, param: Params#Param) : String = {
    "def " + param.toParamName + ": Lens["+className+", "+param.typeName+"] =  Lens.lensu((" + className.toLowerCase + ": " + className+", "+param.toParamName +" : "+param.typeName+") => "+className.toLowerCase+".copy("+param.toParamName+" = "+param.toParamName+"), _."+param.toParamName+")"
  }

  def buildDefComposeLens(className : String, param: Params#Param) : String = {
    "def " + param.toParamName + ": Lens[A, "+param.typeName+"] =  l andThen "+className+"."+param.toParamName
  }

  override def buildObjectLens(localName: String, defLenses: String, defComposeLenses: String): String = {
    newline + "object " + {localName} + " {" + newline +
      indent(1) + defLenses + newline + newline +
      indent(1) + "class " + {localName} + "W[A](l: Lens[A, " + {localName} + "]) {" + newline +
      indent(2) + defComposeLenses + newline + indent(1) + "}" + newline + newline +
      "implicit def lens2" + {localName} + "W[A](l: Lens[A, " + {localName} + "]): " + {localName} + "W[A] = new " + {localName} + "W(l)" +
      newline + "}" + newline
  }