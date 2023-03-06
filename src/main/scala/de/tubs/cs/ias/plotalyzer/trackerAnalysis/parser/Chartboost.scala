package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsValue, JsonParser}

import java.util.Base64
import scala.util.matching.Regex

object Chartboost extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://live.chartboost.com/api/install".r,
    "https://live.chartboost.com/api/config".r,
    "https://live.chartboost.com/banner/show".r,
    "https://live.chartboost.com/webview/v2/prefetch".r,
    "https://live.chartboost.com/webview/v2/reward/get".r,
    "https://live.chartboost.com/webview/v2/interstitial/get".r,
    "https://da.chartboost.com/auction/sdk/banner".r
  )
  override val trackerName: String = "chartboost"
  override val trackingCompany: String = "Chartboost"

  override def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(content) =>
        val json = JsonParser(content)
        val special = List(
          "https://live.chartboost.com/webview/v2/prefetch".r,
          "https://live.chartboost.com/webview/v2/reward/get".r,
          "https://live.chartboost.com/webview/v2/interstitial/get".r,
          "https://da.chartboost.com/auction/sdk/banner".r
        )
        if (special.exists(regexp => regexp.matches(request.getUrl))) {
          val jsObject = json.asJsObject
          val app =
            jsObject.fields.getOrElse("app", JsObject()).asJsObject.fields
          val device =
            jsObject.fields.getOrElse("device", JsObject()).asJsObject.fields
          val sdk =
            jsObject.fields.getOrElse("sdk", JsObject()).asJsObject.fields
          val ad = jsObject.fields.getOrElse("ad", JsObject()).asJsObject.fields
          Some(JsObject(app ++ device ++ sdk ++ ad))
        } else {
          Some(json.asJsObject)
        }
      case None => None
    }
  }

  addExtractor("identity?.gaid") { request =>
    request.getOptionalField[JsString]("identity") match {
      case Some(identity) =>
        val decoded = Base64.getDecoder.decode(identity.value.replace("\n", ""))
        JsonParser(decoded).asJsObject
          .getOptionalField[JsString]("gaid") match {
          case Some(gaid) =>
            Some(GlobalOSAdvertisingIdentifier(gaid.value, BASE64))
          case None => None
        }
      case None => None
    }
  }

  addExtractor("identity?.uuid") { request =>
    request.getOptionalField[JsString]("identity") match {
      case Some(identity) =>
        val decoded = Base64.getDecoder.decode(identity.value.replace("\n", ""))
        JsonParser(decoded).asJsObject
          .getOptionalField[JsString]("uuid") match {
          case Some(uuid) =>
            UUID.createAppropriateIdentifier(uuid.value, BASE64)
          case None => None
        }
      case None => None
    }
  }

  addExtractor("session_id") { request =>
    request.getOptionalField[JsString]("session_id", "session_ID") match {
      case Some(value) => UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None        => None
    }
  }

  addExtractor("device_tyoe") {
    _.withOptionalFieldValue("device_type")(value => Model(value, PLAIN))
  }

  addExtractor("os") {
    _.withOptionalFieldValue("os")(value => OS(value, PLAIN))
  }

  addExtractor("language") {
    _.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  }

  addExtractor("timezone") {
    _.withOptionalFieldValue("timezone")(value => TimeZone(value, PLAIN))
  }

  addExtractor("user_agent") {
    _.withOptionalFieldValue("user_agent")(value => UserAgent(value, PLAIN))
  }

  addExtractor("is_portrait") { request =>
    request.withOptionalFieldValue("is_portrait")(value =>
      Orientation(value, PLAIN))
  }

  addExtractor("carrier") { request =>
    request.getOptionalField[JsValue]("carrier") match {
      case Some(carrier: JsObject) =>
        carrier.withOptionalFieldValue("carrier_name", "carrier-name")(value =>
          Carrier(value, PLAIN))
      case _ => None
    }
  }

  addExtractor("rooted_device") {
    _.withOptionalFieldValue("rooted_device")(value => Rooted(value, PLAIN))
  }

  addExtractor("dw") {
    _.withOptionalFieldValue("dw")(value => Width(value, PLAIN))
  }

  addExtractor("dh") {
    _.withOptionalFieldValue("dh")(value => Height(value, PLAIN))
  }

  addExtractor("country") {
    _.withOptionalFieldValue("country")(value => Country(value, PLAIN))
  }

  addExtractor("bundle_id") {
    _.withOptionalFieldValue("bundle_id")(value => AppID(value, PLAIN))
  }

  addExtractor("bundle") {
    _.withOptionalFieldValue("bundle")(value => AppVersion(value, PLAIN))
  }

  addExtractor("sdk") {
    _.withOptionalFieldValue("sdk")(value => SDKVersion(value, PLAIN))
  }

}
