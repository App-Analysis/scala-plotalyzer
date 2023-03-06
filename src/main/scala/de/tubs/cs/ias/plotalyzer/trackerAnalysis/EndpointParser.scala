package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import spray.json.{JsArray, JsNull, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

import scala.collection.mutable.{Map => MMap}
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex

trait EndpointParser extends LogSupport {

  val endpointURLs: List[Regex]
  val trackerName: String
  val trackingCompany: String

  private val extractors: MMap[String, JsObject => Option[PII]] = MMap()
  private val multiExtractors: MMap[String, JsObject => List[Option[PII]]] =
    MMap()
  protected val requiredMethods: Option[List[String]] = None
  protected def addExtractor(name: String)(
      func: JsObject => Option[PII]): Unit = {
    extractors.addOne(name -> func)
  }

  protected def addMultiExtractor(name: String)(
      func: JsObject => List[Option[PII]]): Unit = {
    multiExtractors.addOne(name -> func)
  }

  protected def parseQueryFromPath(request: Request): Option[JsObject] = {
    request.path.split("\\?").toList match {
      case Nil               => None
      case _ :: Nil          => None
      case _ :: query :: Nil => Some(parseQueryString(query))
      case _ =>
        warn(s"multiple ? detected in ${request.path}")
        None
    }
  }

  protected def parseQueryFromBody(request: Request): Option[JsObject] = {
    Some(parseQueryString(request.content))
  }

  protected def parseQueryString(queryString: String): JsObject = {
    JsObject(
      queryString
        .split("&")
        .toList
        .map { keyValue: String =>
          keyValue.split("=").toList match {
            case Nil =>
              throw new RuntimeException(
                s"weird key value $keyValue in $queryString")
            case key :: Nil          => (key, None)
            case key :: value :: Nil => (key, Some(value))
            case _ =>
              throw new RuntimeException(
                s"weird key value $keyValue in $queryString")
          }
        }
        .groupBy(_._1)
        .map {
          case (key, value) =>
            val jsValue: JsValue =
              value
                .map(_._2)
                .filter(_.nonEmpty)
                .map(
                  elem =>
                    JsString(URLDecoder.decode(elem.get,
                                               StandardCharsets.UTF_8))) match {
                case Nil           => JsNull
                case single :: Nil => single
                case multiple      => JsArray(multiple.toVector)
              }
            URLDecoder.decode(key, StandardCharsets.UTF_8) -> jsValue
        })
  }

  private def methodMatches(request: Request): Boolean = {
    requiredMethods match {
      case Some(methods) =>
        methods.contains(request.method)
      case None => true
    }
  }

  private def endpointMatches(request: Request): Boolean = {
    endpointURLs.exists { regexp =>
      request.host != null && regexp.matches(request.getUrl)
    }
  }

  def requestMatchesEndpoint(request: Request): Boolean = {
    methodMatches(request) && endpointMatches(request)
  }

  def parse(request: Request): List[PII] = {
    //null check required
    try {
      if (methodMatches(request) && endpointMatches(request)) {
        prepare(request) match {
          case Some(value) =>
            extract(value)
              .filter(_.nonEmpty)
              .map(_.get)
              .filter(_.value.nonEmpty)
          case None => List()
        }
      } else {
        List()
      }
    } catch {
      case x: Throwable =>
        try {
          prepare(request) match {
            case Some(prepared) =>
              List(
                RequestParsingIssue(
                  x.getMessage + "\n" + prepared.prettyPrint + x.getStackTrace
                    .mkString("\n", "\n", "\n")))
            case None =>
              throw new RuntimeException(
                "there is nothing prepared still extract throws an error - weird!")
          }
        } catch {
          case _: Throwable =>
            List(
              RequestParsingIssue(
                x.getMessage + x.getStackTrace.mkString("\n", "\n", "\n")))
        }

    }
  }

  protected def prepare(request: Request): Option[JsObject]

  protected def extract(request: JsObject): List[Option[PII]] = {
    extractors.map {
      case (name, extr) =>
        try {
          extr(request)
        } catch {
          case x: Throwable =>
            Some(
              ExtractorFailure(this.trackerName, name, request, x.getMessage))
        }
    }.toList ++ multiExtractors.flatMap {
      case (name, extr) =>
        try {
          extr(request)
        } catch {
          case x: Throwable =>
            List(Some(
              ExtractorFailure(this.trackerName, name, request, x.getMessage)))
        }
    }.toList
  }

}
