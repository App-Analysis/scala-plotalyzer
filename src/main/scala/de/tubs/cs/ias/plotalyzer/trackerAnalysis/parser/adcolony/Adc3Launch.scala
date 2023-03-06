package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.adcolony

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Adc3Launch extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://adc3-launch.adcolony.com/v4/launch".r
  )
  override val trackerName: String = "adcolony [adc3launch]"
  override val trackingCompany: String = "Adcolony"
  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(content) => Some(JsonParser(content).asJsObject)
      case None          => None
    }
  }

  addExtractor("advertiser_id") {
    _.withOptionalFieldValue("advertiser_id")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("device_id") {
    _.withOptionalFieldValue("device_id") { value =>
      UUID(value, PLAIN)
    }
  }

  addExtractor("adc_alt_id") {
    _.withOptionalFieldValue("adc_alt_id") { value =>
      UUID(value, PLAIN)
    }
  }

  addExtractor("carrier_name") {
    _.withOptionalFieldValue("carrier_name")(value => Carrier(value, PLAIN))
  }

  addExtractor("screen_width") {
    _.withOptionalFieldValue("screen_width")(value => Width(value, PLAIN))
  }

  addExtractor("screen_height") {
    _.withOptionalFieldValue("screen_height")(value => Height(value, PLAIN))
  }

  addExtractor("language") {
    _.withOptionalFieldValue("local_language_code", "ln")(value =>
      Language(value, PLAIN))
  }

  addExtractor("mac_address") {
    _.withOptionalFieldValue("mac_address")(value => MacAddress(value, PLAIN))
  }

  addExtractor("manufacturer?.device_model") { request =>
    val manu = request
      .getOptionalField[JsString]("manufacturer", "device_brand")
      .getOrElse(JsString(""))
      .value
    val model = request
      .getOptionalField[JsString]("model", "device_model")
      .getOrElse(JsString(""))
      .value
    val str = manu + model
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("network_type") {
    _.withOptionalFieldValue("network_type")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("os") { request =>
    val os = request
      .getOptionalField[JsString]("os_name", "platform")
      .getOrElse(JsString(""))
      .value
    val version =
      request
        .getOptionalField[JsString]("os_version")
        .getOrElse(JsString(""))
        .value
    val str = os + version
    if (str.nonEmpty)
      Some(OS(str, PLAIN))
    else
      None
  }

  addExtractor("architecture") {
    _.withOptionalFieldValue("architecture")(value =>
      Architecture(value, PLAIN))
  }

  addExtractor("battery_level") {
    _.withOptionalFieldValue("battery_level")(value =>
      BatteryPercentage(value, PLAIN))
  }

  addExtractor("timezone_ietf") {
    _.withOptionalFieldValue("timezone_ietf")(value => TimeZone(value, PLAIN))
  }

  addExtractor("current_orientation") {
    _.withOptionalFieldValue("current_orientation")(value =>
      Orientation(value, PLAIN))
  }

  addExtractor("dark_mode") {
    _.withOptionalFieldValue("dark_mode")(value => DarkMode(value, PLAIN))
  }

}
