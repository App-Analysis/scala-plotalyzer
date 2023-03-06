package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsNumber, JsObject, JsString, JsValue, JsonParser}

import scala.util.matching.Regex

object Branchio extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://api(2)?.branch.io/v1/install".r,
    "https://api(2)?.branch.io/v1/open".r,
    "https://api(2)?.branch.io/v1/profile".r,
    "https://api(2)?.branch.io/v1/logout".r
  )

  override val trackerName: String = "Branchio"
  override val trackingCompany: String = "branch.io"

  override protected def prepare(request: Request): Option[JsObject] = {
    if (request.content.startsWith("{")) {
      Some(JsonParser(request.content).asJsObject)
    } else {
      None
    }
  }

  addExtractor("idfa") { request =>
    request.getOptionalField[JsString]("google_advertising_id") match {
      case Some(value) =>
        Some(GlobalOSAdvertisingIdentifier(value.value, PLAIN))
      case None =>
        request.getOptionalField[JsObject]("advertising_ids") match {
          case Some(value) =>
            value.withOptionalFieldValue("aaid")(value =>
              GlobalOSAdvertisingIdentifier(value, PLAIN))
          case None => None
        }
    }
  }

  addExtractor("idfv") { request =>
    request.withOptionalFieldValue("ios_vendor_id")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("hardware_id") { request =>
    request.getOptionalField[JsString]("hardware_id") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("metadata?.marketing_cloud_visitor_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("marketing_cloud_visitor_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.braze_install_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("braze_install_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.device_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("device_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.uuid") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.getOptionalField[JsString]("uuid") match {
          case Some(value) =>
            UUID.createAppropriateIdentifier(value.value, PLAIN)
          case None => None
        }
      case None => None
    }
  }

  addExtractor("metadata?.google_analytics_client_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("google_analytics_client_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.mixpanel_distinct_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("mixpanel_distinct_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.segment_anonymous_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("segment_anonymous_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.transaction_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("transaction_id")(value =>
          UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("metadata?.marketing_cloud_visitor_id") { request =>
    request.getOptionalField[JsObject]("metadata") match {
      case Some(metadata) =>
        metadata.withOptionalFieldValue("user_id")(value => UUID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("UDID") { request =>
    request.withOptionalFieldValue("UDID")(value => UUID(value, PLAIN))
  }

  addExtractor("device_fingerprint_id") { request =>
    request.withOptionalFieldValue("device_fingerprint_id")(value =>
      OtherIdentifier(value, PLAIN))
  }

  addExtractor("identity_id") { request =>
    request.withOptionalFieldValue("identity_id")(value =>
      OtherIdentifier(value, PLAIN))
  }

  addExtractor("model") { request =>
    val brand = request.getOptionalField[JsString]("brand") match {
      case Some(value) => value.value
      case None        => ""
    }
    val model = request.getOptionalField[JsString]("model") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = brand + model
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("width") { request =>
    request.withOptionalFieldValue("width")(value => Width(value, PLAIN))
  }

  addExtractor("height") { request =>
    request.withOptionalFieldValue("height")(value => Height(value, PLAIN))
  }

  addExtractor("connection_type") { request =>
    request.withOptionalFieldValue("connection_type")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("os") { request =>
    val os: String =
      request.getOptionalField[JsString]("os").getOrElse(JsString("")).value
    val osVersionAndroid: String = request
      .getOptionalField[JsValue]("os_version_android") match {
      case Some(x: JsString) => x.value
      case Some(x: JsNumber) => x.value.toString()
      case _                 => ""
    }
    val apiLevel = request.getOptionalField[JsValue]("os_version") match {
      case Some(x: JsString) => x.value
      case Some(x: JsNumber) => x.value.toString()
      case _                 => ""
    }
    val str = os + osVersionAndroid + apiLevel
    if (str.nonEmpty)
      Some(OS(str, PLAIN))
    else
      None
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("language", "locale")(value =>
      Language(value, PLAIN))
  }

  addExtractor("local_ips") { request =>
    request.withOptionalFieldValue("local_ip")(value => LocalIp(value, PLAIN))
  }

  addExtractor("architecture") { request =>
    request.withOptionalFieldValue("cpu_type")(value =>
      Architecture(value, PLAIN))
  }

  addExtractor("carrier") { request =>
    request.withOptionalFieldValue("device_carrier")(value =>
      Carrier(value, PLAIN))
  }

  addExtractor("user_agent") { request =>
    request.withOptionalFieldValue("user_agent")(value =>
      UserAgent(value, PLAIN))
  }

}
