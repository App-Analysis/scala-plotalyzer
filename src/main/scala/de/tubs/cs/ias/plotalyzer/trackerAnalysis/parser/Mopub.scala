package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Mopub extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://ads.mopub.com/m/open".r,
    "https://ads.mopub.com/m/gdpr_sync".r,
    "https://ads.mopub.com/m/ad".r,
  )
  override val trackerName: String = "mopub"
  override val trackingCompany: String = "Mobpub"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(value) if value.nonEmpty => Some(JsonParser(value).asJsObject)
      case None                          => None
    }
  }

  addExtractor("app_version") { request =>
    request.withOptionalFieldValue("app_version")(value =>
      AppVersion(value, PLAIN))
  }

  addExtractor("bundle") { request =>
    request.withOptionalFieldValue("bundle", "id")(value => AppID(value, PLAIN))
  }

  addExtractor("version") { request =>
    request.withOptionalFieldValue("nv")(value => SDKVersion(value, PLAIN))
  }

  addExtractor("idfa") { request =>
    request.getOptionalField[JsString]("udid") match {
      case Some(value) if value.value.startsWith("ifa:") =>
        Some(GlobalOSAdvertisingIdentifier(value.value, PLAIN))
      case _ =>
        request.getOptionalField[JsString]("consent_ifa") match {
          case Some(value) =>
            Some(GlobalOSAdvertisingIdentifier(value.value, PLAIN))
          case None => None
        }
    }
  }

  addExtractor("idfv") { request =>
    request.withOptionalFieldValue("ifv")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("udid") { request =>
    request.withOptionalFieldValue("udid") { value =>
      if (value.startsWith("mopub:")) {
        UUID(value.substring("mopub:".length), PLAIN)
      } else {
        UUID(value, PLAIN)
      }
    }
  }

  addExtractor("os") { request =>
    val os =
      request.getOptionalField[JsString]("os").getOrElse(JsString("")).value
    val osv =
      request.getOptionalField[JsString]("osv").getOrElse(JsString("")).value
    val str = os + osv
    if (str.nonEmpty)
      Some(OS(os + osv, PLAIN))
    else
      None
  }

  addExtractor("model") { request =>
    request.getOptionalField[JsString]("dn") match {
      case Some(value) =>
        Some(Model(value.value, PLAIN))
      case None =>
        val maker = request
          .getOptionalField[JsString]("make", "hwv")
          .getOrElse(JsString(""))
          .value
        val model = request
          .getOptionalField[JsString]("model")
          .getOrElse(JsString(""))
          .value
        val str = maker + model
        if (str.nonEmpty)
          Some(Model(str, PLAIN))
        else
          None
    }
  }

  addExtractor("timezone") { request =>
    request.withOptionalFieldValue("z")(value => TimeZone(value, PLAIN))
  }

  addExtractor("carrier") { request =>
    request.withOptionalFieldValue("cn")(value => Carrier(value, PLAIN))
  }

  addExtractor("height") { request =>
    request.withOptionalFieldValue("h")(value => Height(value, PLAIN))
  }

  addExtractor("width") { request =>
    request.withOptionalFieldValue("w")(value => Width(value, PLAIN))
  }

}
