package scalaxb.servlet

import java.net.{URI, URISyntaxException}
import java.io.{File}
import scala.collection.mutable
import scalaxb.servlet.model.{Driver, ScalaFile, SchemaFile}
import scalaxb.compiler.{Config}
import unfiltered.request._
import unfiltered.response._
import unfiltered.request.{Path => UFPath}

class CompilerFilter extends unfiltered.filter.Planify ({
  case POST(UFPath(Seg("compile" :: what :: Nil), Params(params0, req))) =>
    val (params, multipart) = req match {
      case MultiPart(req) =>
        val data = MultiPartParams.Memory(req)
        (data.params, data.files("argf") filter {_.size > 0})
      case _ => (params0, Nil)
    }
    def nonempty(s: String) = params(s) filter { "" != }
    def opt(s: String) = nonempty(s).headOption
    
    try {
      lazy val files = (nonempty("arg") map { x => SchemaFile.fromURI(new URI(x)) }) ++
        (multipart map { x => SchemaFile.fromBytes(x.name, x.bytes) })
      
      lazy val config = Config(packageNames = Map[Option[String], Option[String]](
          (None -> opt("defaultPackageName")) ::
          (params("namespaceURI") zip params("packageName")).toList.map { x =>
            Some(x._1) -> Some(x._2) }:_*),
        classPrefix = opt("classPrefix"),
        paramPrefix = opt("paramPrefix"),
        wrappedComplexTypes = nonempty("wrapContents").toList)
      
      lazy val scalaFiles = Driver.process(files, config)
      
      if (files.isEmpty) BadRequest ~> ResponseString("missing arg.")
      else ContentType("application/octet-stream") ~> 
        ResponseHeader("Content-disposition",
          Seq("attachment; filename=\"" + what + "\"")) ~>
        ("""([.]\w+)$""".r.findFirstIn(what) match {
          case Some(".scala") => ResponseString(scalaFiles.head.content)
          case Some(".zip") => ResponseBytes(ScalaFile.zip(scalaFiles))
          case _ => BadRequest ~> ResponseString("unsupported file extension.")
        })
    }
    catch {
      case e: URISyntaxException => BadRequest ~> ResponseString(e.getMessage)
      case e: Exception => InternalServerError ~>
        ResponseString(e.getMessage + "\n\n" +
          e.getStackTrace.toArray.mkString("\n"))
    }
})
