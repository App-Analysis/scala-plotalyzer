package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsObject, JsString}

import scala.util.matching.Regex

object Yandex extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://startup.mobile.yandex.net/analytics/startup".r,
    "https://report.appmetrica.yandex.net/report".r,
    "https://proxy.mob.maps.yandex.net/mapkit2/.*".r
  )
  override val trackerName: String = "yandex"
  override val trackingCompany: String = "Yandex"

  override protected def prepare(request: Request): Option[JsObject] = {
    parseQueryFromPath(request)
  }

  addExtractor("lang") {
    _.withOptionalFieldValue("lang")(value => Language(value, PLAIN))
  }

  addExtractor("uuid") {
    _.withOptionalFieldValue("uuid")(value => UUID(value, PLAIN))
  }

  addExtractor("ifa") { request =>
    request.withOptionalFieldValue("adv_id", "ifa")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("ifv") { request =>
    request.withOptionalFieldValue("ifv")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("deviceid") { request =>
    request.withOptionalFieldValue("deviceid")(value =>
      OtherIdentifier(value, PLAIN))
  }

  addExtractor("deviceid2") { request =>
    request.withOptionalFieldValue("deviceid2")(value => UUID(value, PLAIN))
  }

  addExtractor("android_id") { request =>
    request.getOptionalField[JsString]("android_id") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("yandex_adv_id") { request =>
    request.withOptionalFieldValue("yandex_adv_id")(value => UUID(value, PLAIN))
  }

  addExtractor("os") { request =>
    val platform = request
      .getOptionalField[JsString]("app_platform")
      .getOrElse(JsString(""))
      .value
    val version = request
      .getOptionalField[JsString]("os_version")
      .getOrElse(JsString(""))
      .value
    val str = platform + version
    if (str.nonEmpty)
      Some(OS(platform + version, PLAIN))
    else
      None
  }

  addExtractor("model") { request =>
    val manu = request
      .getOptionalField[JsString]("manufacturer")
      .getOrElse(JsString(""))
      .value
    val model = request
      .getOptionalField[JsString]("manufacturer")
      .getOrElse(JsString(""))
      .value
    val str = manu + model
    if (str.nonEmpty)
      Some(Model(manu + model, PLAIN))
    else
      None
  }

  addExtractor("width") { request =>
    request.withOptionalFieldValue("screen_width")(value => Width(value, PLAIN))
  }

  addExtractor("height") { request =>
    request.withOptionalFieldValue("screen_height")(value =>
      Height(value, PLAIN))
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("locale") { locale =>
      Language(locale.split("_").head, PLAIN)
    }
  }

  addExtractor("rooted") { request =>
    request.withOptionalFieldValue("is_rooted")(value => Rooted(value, PLAIN))
  }

  addExtractor("sdk_version") { request =>
    request.withOptionalFieldValue("analytics_sdk_version_name")(value =>
      SDKVersion(value, PLAIN))
  }

  addExtractor("app_id") { request =>
    request.withOptionalFieldValue("app_id")(value => AppID(value, PLAIN))
  }

  addExtractor("app_version") { request =>
    request.withOptionalFieldValue("app_version_name")(value =>
      AppVersion(value, PLAIN))
  }
}
