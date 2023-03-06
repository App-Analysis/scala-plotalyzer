package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{
  AppID,
  AppVersion,
  Country,
  EndpointParser,
  GlobalOSAdvertisingIdentifier,
  Height,
  Language,
  LocalOSAdvertisingIdentifier,
  Model,
  OS,
  UUID,
  Width
}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsObject, JsString, JsonParser}

import java.net.URLDecoder
import scala.util.matching.Regex

object Adjust extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://app(.eu)?.adjust.(com|net.in|world)/session".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/attribution".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/event".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/sdk_click".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/sdk_info".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/third_party_sharing".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/ad_revenue".r,
    "https://app(.eu)?.adjust.(com|net.in|world)/sdk_click".r
  )
  override val trackerName: String = "adjust"
  override val trackingCompany: String = "Adjust"

  override protected def prepare(request: Request): Option[JsObject] = {
    val json = if (request.getUrl.endsWith("attribution")) {
      parseQueryFromPath(request)
    } else {
      Option(request.content) match {
        case Some(_) => parseQueryFromBody(request) //session,sdk_click
        case None    => None
      }
    }
    val ret = json match {
      case Some(reqJson) =>
        val callback_params: JsObject =
          reqJson.fields.get("callback_params") match {
            case Some(value: JsString) =>
              JsonParser(URLDecoder.decode(value.value)).asJsObject
            case _ => JsObject()
          }
        val partner_params: JsObject =
          reqJson.fields.get("partner_params") match {
            case Some(value: JsString) =>
              JsonParser(URLDecoder.decode(value.value)).asJsObject
            case _ => JsObject()
          }
        Some(DeepMerge.merge(reqJson, callback_params, partner_params))
      case None => None
    }
    ret
  }

  addExtractor("package_name") {
    _.withOptionalFieldValue("package_name", "bundle_id")(value =>
      AppID(value, PLAIN))
  }

  addExtractor("app_version") {
    _.withOptionalFieldValue("app_version")(value => AppVersion(value, PLAIN))
  }

  addExtractor("gps_adid") {
    _.withOptionalFieldValue("gps_adid", "idfa")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("idfv") {
    _.withOptionalFieldValue("idfv")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("android_uuid") {
    _.withOptionalFieldValue("android_uuid")(value => UUID(value, PLAIN))
  }

  addExtractor("ios_uuid") {
    _.withOptionalFieldValue("ios_uuid")(value => UUID(value, PLAIN))
  }

  addExtractor("fb_anon_id") { request =>
    request.getOptionalField[JsString]("fb_anon_id") match {
      case Some(value) => UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None        => None
    }
  }

  addExtractor("language")(
    _.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  )

  addExtractor("model") { request =>
    val manu = request
      .getOptionalField[JsString]("device_manufacturer")
      .getOrElse(JsString(""))
      .value
    val name = request
      .getOptionalField[JsString]("device_name")
      .getOrElse(JsString(""))
      .value
    val str = manu + name
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("display_width") {
    _.withOptionalFieldValue("display_width")(value => Width(value, PLAIN))
  }

  addExtractor("display_height") {
    _.withOptionalFieldValue("display_height")(value => Height(value, PLAIN))
  }

  addExtractor("country") {
    _.withOptionalFieldValue("country")(value => Country(value, PLAIN))
  }

  addExtractor("os") { request =>
    val name = request
      .getOptionalField[JsString]("os_name")
      .getOrElse(JsString(""))
      .value
    val os_version = request
      .getOptionalField[JsString]("os_name")
      .getOrElse(JsString(""))
      .value
    val os_build = request
      .getOptionalField[JsString]("os_name")
      .getOrElse(JsString(""))
      .value
    val str = name + os_version + os_build
    if (str.nonEmpty) {
      Some(OS(str, PLAIN))
    } else {
      None
    }
  }
}
