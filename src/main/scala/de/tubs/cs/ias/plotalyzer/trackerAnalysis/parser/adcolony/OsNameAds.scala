package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.adcolony

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object OsNameAds extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://(android|ios)?ads\\d-?\\d.adcolony.com/configure".r
  )
  override val trackerName: String = "adcolony [OsNameAds]"
  override val trackingCompany: String = "Adcolony"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(content) =>
        Some(JsonParser(content).asJsObject)
      case None =>
        None
    }
  }

  addExtractor("appId") { request =>
    request.withOptionalFieldValue("bundle_id") { value =>
      AppID(value, PLAIN)
    }
  }

  addExtractor("appVersion") { request =>
    request.withOptionalFieldValue("bundle_version_short")(value =>
      AppID(value, PLAIN))
  }

  addExtractor("os") { request =>
    val name = request.getOptionalField[JsString]("os_name") match {
      case Some(value) => value.value
      case None        => ""
    }
    val version = request.getOptionalField[JsString]("os_version") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = name + version
    if (str.nonEmpty) {
      Some(OS(str, PLAIN))
    } else {
      None
    }
  }

  addExtractor("idfa") { request =>
    request.withOptionalFieldValue("advertiser_id")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("idfv") { request =>
    request.withOptionalFieldValue("vendor_id")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("sid") { request =>
    request.withOptionalFieldValue("sid") { value =>
      UUID(value, PLAIN)
    }
  }

  addExtractor("carrier") { request =>
    request.withOptionalFieldValue("carrier")(value => Carrier(value, PLAIN))
  }

  addExtractor("model") { request =>
    val brand =
      request.getOptionalField[JsString]("device_brand", "manufacturer") match {
        case Some(value) => value.value
        case None        => ""
      }
    val model = request.getOptionalField[JsString]("device_model") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = brand + model
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("battery_level") { request =>
    request.withOptionalFieldValue("battery_level")(value =>
      BatteryPercentage(value, PLAIN))
  }

  addExtractor("current_orientation") { request =>
    request.withOptionalFieldValue("current_orientation")(value =>
      Orientation(value, PLAIN))
  }

  addExtractor("timezone_ietf") { request =>
    request.withOptionalFieldValue("timezone_ietf")(value =>
      TimeZone(value, PLAIN))
  }

  addExtractor("height") { request =>
    request.withOptionalFieldValue("screen_height")(value =>
      Height(value, PLAIN))
  }

  addExtractor("width") { request =>
    request.withOptionalFieldValue("screen_width")(value => Width(value, PLAIN))
  }

}
