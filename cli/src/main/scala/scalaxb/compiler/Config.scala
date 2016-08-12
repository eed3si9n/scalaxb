/*
 * Copyright (c) 2015 e.e d3si9n
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

import scalashim._
import java.io.File
import scala.collection.immutable.{ Map, Set, Seq }

case class Config(items: Map[String, ConfigEntry]) {
  import Config._
  import ConfigEntry._
  def values: Vector[ConfigEntry] = items.values.toVector
  def packageNames: Map[Option[String], Option[String]] =
    (get[PackageNames] getOrElse defaultPackageNames).value
  def classPrefix: Option[String] =
    get[ClassPrefix] map {_.value}
  def classPostfix: Option[String] =
    get[ClassPostfix] map {_.value}
  def paramPrefix: Option[String] =
    get[ParamPrefix] map {_.value}
  def attributePrefix: Option[String] =
    get[AttributePrefix] map {_.value}
  def outdir: File =
    (get[Outdir] getOrElse defaultOutdir).value
  def packageDir: Boolean = values contains GeneratePackageDir
  def wrappedComplexTypes: List[String] =
    (get[WrappedComplexTypes] getOrElse defaultWrappedComplexTypes).value
  def prependFamilyName: Boolean = values contains PrependFamilyName
  def seperateProtocol: Boolean = values contains SeperateProtocol
  def protocolFileName: String =
    (get[ProtocolFileName] getOrElse defaultProtocolFileName).value
  def protocolPackageName: Option[String] =
    (get[ProtocolPackageName] getOrElse defaultProtocolPackageName).value
  def defaultNamespace: Option[String] =
    (get[DefaultNamespace] getOrElse defaultDefaultNamespace).value
  def generateRuntime: Boolean = values contains GenerateRuntime
  def generateDispatchClient: Boolean = values contains GenerateDispatchClient
  def generateDispatchAs: Boolean = values contains GenerateDispatchAs
  def contentsSizeLimit: Int =
    (get[ContentsSizeLimit] getOrElse defaultContentsSizeLimit).value
  def sequenceChunkSize: Int =
    (get[SequenceChunkSize] getOrElse defaultSequenceChunkSize).value
  def namedAttributes: Boolean = values contains NamedAttributes
  def laxAny: Boolean = values contains LaxAny
  def async: Boolean = values contains GenerateAsync
  def dispatchVersion: String =
    (get[DispatchVersion] getOrElse defaultDispatchVersion).value
  def varArg: Boolean = values contains VarArg
  def ignoreUnknown: Boolean = values contains IgnoreUnknown
  def autoPackages: Boolean = values contains AutoPackages

  private def get[A <: ConfigEntry: Manifest]: Option[A] =
    items.get(implicitly[Manifest[A]].runtimeClass.getName).asInstanceOf[Option[A]]
  def update(item: ConfigEntry): Config =
    copy(items = items.updated(item.name, item))
  def remove(item: ConfigEntry): Config =
    copy(items = items - item.name)
}

object Config {
  import ConfigEntry._

  def apply(xs: Vector[ConfigEntry]): Config =
    xs.foldLeft(new Config(Map())) { (acc, x) => acc.update(x) }
  val defaultPackageNames = PackageNames(Map(None -> None))
  val defaultOutdir = Outdir(new File("."))
  val defaultWrappedComplexTypes = WrappedComplexTypes(Nil)
  val defaultProtocolFileName = ProtocolFileName("xmlprotocol.scala")
  val defaultProtocolPackageName = ProtocolPackageName(None)
  val defaultDefaultNamespace = DefaultNamespace(None)
  val defaultContentsSizeLimit = ContentsSizeLimit(Int.MaxValue)
  val defaultSequenceChunkSize = SequenceChunkSize(10)
  val defaultDispatchVersion = DispatchVersion(scalaxb.BuildInfo.defaultDispatchVersion)

  val default = Config(
    Vector(defaultPackageNames, defaultOutdir, defaultWrappedComplexTypes,
      SeperateProtocol, defaultProtocolFileName, defaultProtocolPackageName,
      GenerateRuntime, GenerateDispatchClient,
      defaultContentsSizeLimit, defaultSequenceChunkSize,
      GenerateAsync, defaultDispatchVersion, VarArg)
  )
}

sealed trait ConfigEntry {
  def name: String = getClass.getName
}
object ConfigEntry {
  case class PackageNames(value: Map[Option[String], Option[String]]) extends ConfigEntry
  case class ClassPrefix(value: String) extends ConfigEntry
  case class ClassPostfix(value: String) extends ConfigEntry
  case class ParamPrefix(value: String) extends ConfigEntry
  case class AttributePrefix(value: String) extends ConfigEntry
  case class Outdir(value: File) extends ConfigEntry
  case object GeneratePackageDir extends ConfigEntry
  case class WrappedComplexTypes(value: List[String]) extends ConfigEntry
  case object PrependFamilyName extends ConfigEntry
  case object SeperateProtocol extends ConfigEntry
  case class ProtocolFileName(value: String) extends ConfigEntry
  case class ProtocolPackageName(value: Option[String]) extends ConfigEntry
  case class DefaultNamespace(value: Option[String]) extends ConfigEntry
  case object GenerateRuntime extends ConfigEntry
  case object GenerateDispatchClient extends ConfigEntry
  case object GenerateDispatchAs extends ConfigEntry
  case class ContentsSizeLimit(value: Int) extends ConfigEntry
  case class SequenceChunkSize(value: Int) extends ConfigEntry
  case object NamedAttributes extends ConfigEntry
  case object LaxAny extends ConfigEntry
  case object GenerateAsync extends ConfigEntry
  case class DispatchVersion(value: String) extends ConfigEntry
  case object VarArg extends ConfigEntry
  case object IgnoreUnknown extends ConfigEntry
  case object AutoPackages                                    extends ConfigEntry
}
