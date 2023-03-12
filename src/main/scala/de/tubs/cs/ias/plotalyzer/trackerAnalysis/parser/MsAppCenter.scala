package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.utility.json.DeepMerge
import spray.json.{JsArray, JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object MsAppCenter extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://in.appcenter.ms/logs".r,
  )
  override val trackerName: String = "ms appcenter"
  override val trackingCompany: String = "Microsoft"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(content) =>
        Some(
          DeepMerge.merge(
            JsonParser(content).asJsObject
              .fields("logs")
              .asInstanceOf[JsArray]
              .elements
              .map(_.asJsObject): _*))
      case None => None
    }
  }

  addExtractor("appid") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("appNamespace")(value =>
          AppID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("appversion") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("appVersion")(value =>
          AppVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("sdkVersion") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("sdkVersion")(value =>
          SDKVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("locale") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("locale")(value => Language(value, PLAIN))
      case None => None
    }
  }

  addExtractor("timeZoneOffset") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("timeZoneOffset")(value =>
          TimeZone(value, PLAIN))
      case None => None
    }
  }

  addExtractor("carrierName") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("carrierName")(value =>
          Carrier(value, PLAIN))
      case None => None
    }
  }

  addExtractor("sid") { request =>
    request.withOptionalFieldValue("sid")(value => UUID(value, PLAIN))
  }

  addExtractor("userId") { request =>
    request.withOptionalFieldValue("userId")(value =>
      OtherIdentifier(value, PLAIN))
  }

  addMultiExtractor("screen") { request =>
    request
      .getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.getOptionalField[JsString]("screenSize") match {
          case Some(screen) =>
            List(
              Some(Width(screen.value.split("x").head, PLAIN)),
              Some(Height(screen.value.split("x").tail.head, PLAIN))
            )
          case None => List()
        }
      case None => List()
    }

  }

  addExtractor("os") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        val os = device
          .getOptionalField[JsString]("osName")
          .getOrElse(JsString(""))
          .value
        val osv = device
          .getOptionalField[JsString]("osVersion")
          .getOrElse(JsString(""))
          .value
        val str = os + osv
        if (str.nonEmpty)
          Some(OS(str, PLAIN))
        else
          None
      case None => None
    }
  }

}
