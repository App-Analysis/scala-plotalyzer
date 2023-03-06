package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsArray, JsNull, JsObject, JsString, JsValue, JsonParser}

import scala.util.matching.Regex

object OneSignal extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://api.onesignal.com/players".r,
    "https://onesignal.com/api/v1/players".r,
    "https://api.onesignal.com/players/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}".r,
    "https://api.onesignal.com/in_app_messages/.*".r
  )
  override val trackerName: String = "onesignal"
  override val trackingCompany: String = "OneSignal"

  override protected def prepare(request: Request): Option[JsObject] = {
    val playerId = request.path.split("/players/").toList match {
      case _ :: rhs :: Nil =>
        JsString(rhs.split("/").head.split('?').head)
      case _ => JsNull
    }
    try {
      val newMap
        : Map[String, JsValue] = JsonParser(request.content).asJsObject.fields ++ Map(
        "player_id" -> playerId)
      Some(JsObject(newMap))
    } catch {
      case _: Throwable if playerId == JsNull => None
      case _: Throwable                       => Some(JsObject("player_id" -> playerId))
    }
  }

  addExtractor("sdk") { request =>
    request.withOptionalFieldValue("sdk") { value =>
      SDKVersion(value, PLAIN)
    }
  }

  addExtractor("appid") { request =>
    request.withOptionalFieldValue("android_package", "ios_bundle", "app_id")(
      value => AppID(value, PLAIN))
  }

  addExtractor("add_id") { request =>
    request.withOptionalFieldValue("ad_id")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("device?.device_id") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.getOptionalField[JsString]("device_id") match {
          case Some(value) =>
            UUID.createAppropriateIdentifier(value.value, PLAIN)
          case None => None
        }
      case None => None
    }
  }

  addExtractor("identifier") { request =>
    request.getOptionalField[JsString]("identifier") match {
      case Some(value) => UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None        => None
    }
  }

  addExtractor("external_user_id") { request =>
    request.getOptionalField[JsString]("external_user_id") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("tags?.device_id") { request =>
    request.getOptionalField[JsObject]("tags") match {
      case Some(tags) =>
        tags.getOptionalField[JsString]("device_id") match {
          case Some(value) =>
            UUID.createAppropriateIdentifier(value.value, PLAIN)
          case None => None
        }
      case None => None
    }
  }

  addExtractor("player_id") { request =>
    request.withOptionalFieldValue("player_id")(value => UUID(value, PLAIN))
  }

  addExtractor("os") { request =>
    request.getOptionalField[JsValue]("device_os") match {
      case Some(x: JsArray) =>
        Some(
          OS(x.elements.map(_.asInstanceOf[JsString].value).mkString(" "),
             PLAIN))
      case Some(x: JsString) =>
        Some(OS(x.value, PLAIN))
      case _ => None
    }
  }

  addExtractor("timezone_id") { request =>
    request.withOptionalFieldValue("timezone_id")(value =>
      TimeZone(value, PLAIN))
  }

  addExtractor("device_model") { request =>
    request.withOptionalFieldValue("device_model")(value => Model(value, PLAIN))
  }

  addExtractor("carrier") { request =>
    request.withOptionalFieldValue("carrier")(value => Carrier(value, PLAIN))
  }

  addExtractor("rooted") { request =>
    request.withOptionalFieldValue("rooted")(value => Rooted(value, PLAIN))
  }

  addExtractor("language") { request =>
    request.getOptionalField[JsObject]("tags") match {
      case Some(value) =>
        value.withOptionalFieldValue("lang", "language")(value =>
          Language(value, PLAIN))
      case None =>
        request.withOptionalFieldValue("language")(value =>
          Language(value, PLAIN))
    }
  }

  addExtractor("device_name") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("deviceName")(value =>
          DeviceName(value, PLAIN))
      case None => None
    }
  }

  addExtractor("lat") { request =>
    request.withOptionalFieldValue("lat")(value => Latitude(value, PLAIN))
  }

  addExtractor("long") { request =>
    request.withOptionalFieldValue("long")(value => Longitude(value, PLAIN))
  }
}
