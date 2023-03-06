package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsObject, JsString, JsValue}

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex

object Rayjump extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://configure.rayjump.com/setting".r,
    "https://analytics.rayjump.com".r
  )
  override val trackerName: String = "rayjump"
  override val trackingCompany: String = "Rayjump"

  override protected def prepare(request: Request): Option[JsObject] = {
    if (endpointURLs.head.matches(request.getUrl)) {
      parseQueryFromPath(request)
    } else {
      Option(request.content) match {
        case Some(content) =>
          val json = parseQueryString(
            URLDecoder.decode(content, StandardCharsets.UTF_8))
          Some(
            DeepMerge
              .merge(json,
                     parseQueryString(json.getField[JsString]("data").value)))
        case None => None
      }
    }
  }

  addExtractor("os") { request =>
    val os = request.getOptionalField[JsString]("os", "db")
    val osv = request.getOptionalField[JsString]("os_version", "osv")
    (os, osv) match {
      case (Some(os), Some(osv)) => Some(OS(os.value + osv.value, PLAIN))
      case (Some(os), None)      => Some(OS(os.value, PLAIN))
      case (None, Some(osv))     => Some(OS(osv.value, PLAIN))
      case (None, None)          => None
    }
  }

  addExtractor("orientation") { request =>
    request.withOptionalFieldValue("orientation")(value =>
      Orientation(value, PLAIN))
  }

  addExtractor("model") { request =>
    val brand =
      request.getOptionalField[JsString]("brand").getOrElse(JsString("")).value
    val model =
      request.getOptionalField[JsString]("model").getOrElse(JsString("")).value
    val str = brand + model
    if (str.nonEmpty)
      Some(OS(str, PLAIN))
    else
      None
  }

  addExtractor("idfa") { request =>
    (request.getOptionalField[JsValue]("data") match {
      case Some(data: JsObject) =>
        data.withOptionalFieldValue("gaid")(value =>
          GlobalOSAdvertisingIdentifier(value, PLAIN))
      case _ => None
    }) match {
      case Some(value) => Some(value)
      case None =>
        request.withOptionalFieldValue("gaid", "idfa")(value =>
          GlobalOSAdvertisingIdentifier(value, PLAIN))
    }
  }

  addExtractor("idfv") { request =>
    request.withOptionalFieldValue("idfv")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  }

  addExtractor("timezone") { request =>
    request.withOptionalFieldValue("timezone")(value => TimeZone(value, PLAIN))
  }

  addExtractor("useragent") { request =>
    request.withOptionalFieldValue("useragent", "ua")(value =>
      UserAgent(value, PLAIN))
  }

  addMultiExtractor("screen_size") { request =>
    request.getOptionalField[JsString]("screen_size") match {
      case Some(value) =>
        List(
          Some(Width(value.value.split("x").head, PLAIN)),
          Some(Height(value.value.split("x").tail.head, PLAIN))
        )
      case None => List()
    }
  }

  addExtractor("appid") { request =>
    request.withOptionalFieldValue("package_name", "pn")(value =>
      AppID(value, PLAIN))
  }

  addExtractor("appversion") { request =>
    request.withOptionalFieldValue("app_version_name")(value =>
      AppVersion(value, PLAIN))
  }

  addExtractor("sdk_version") { request =>
    request.withOptionalFieldValue("sdk_version")(value =>
      SDKVersion(value, PLAIN))
  }

  addExtractor("country") { request =>
    request.withOptionalFieldValue("ct", "country_code")(value =>
      Country(value, PLAIN))
  }

  addExtractor("ip") { request =>
    request.withOptionalFieldValue("ip")(value => PublicIP(value, PLAIN))
  }
}
