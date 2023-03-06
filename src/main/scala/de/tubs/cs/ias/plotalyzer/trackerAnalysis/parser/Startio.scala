package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}
import wvlet.log.LogSupport

import scala.util.matching.Regex

object Startio extends EndpointParser with LogSupport {

  override val endpointURLs: List[Regex] = List(
    "https://infoevent.startappservice.com/tracking/infoEvent".r,
    "https://infoevent.startappservice.com/infoevent/api/v1.0/info".r,
    "https://trackdownload.startappservice.com/trackdownload/api/1.0/trackdownload".r
  )

  override val trackerName: String = "startio"
  override val trackingCompany: String = "start.io"

  override protected def prepare(request: Request): Option[JsObject] = {
    if (request.getUrl == "https://trackdownload.startappservice.com/trackdownload/api/1.0/trackdownload") {
      request.path.split('?').toList match {
        case Nil => None
        case _ :: query :: Nil =>
          Some(parseQueryString(query))
        case _ :: query =>
          warn(s"url of ${request.getFullUrl} is badly formed and has two ?")
          val reconstructed = query.mkString("?")
          Some(parseQueryString(reconstructed))
      }
    } else {
      Some(JsonParser(request.content).asJsObject)
    }
  }

  addExtractor("packageId") { request =>
    request.withOptionalFieldValue("packageId")(value => AppID(value, PLAIN))
  }

  addExtractor("appVersion") { request =>
    request.withOptionalFieldValue("appVersion")(value =>
      AppVersion(value, PLAIN))
  }

  addExtractor("appActivity") { request =>
    request.withOptionalFieldValue("appActivity")(value =>
      CurrentlyViewed(value, PLAIN))
  }

  addExtractor("fgApp") { request =>
    request.withOptionalFieldValue("fgApp")(value => InForeground(value, PLAIN))
  }

  addExtractor("sdkVersion") { request =>
    request.withOptionalFieldValue("sdkVersion")(value =>
      SDKVersion(value, PLAIN))
  }

  addExtractor("uuids") { request =>
    request.withOptionalFieldValue("clientSessionId")(value =>
      UUID(value, PLAIN))
  }

  addExtractor("os") { request =>
    val os =
      request.getOptionalField[JsString]("os").getOrElse(JsString("")).value
    val version = request
      .getOptionalField[JsString]("deviceVersion")
      .getOrElse(JsString(""))
      .value
    val str = os + version
    if (str.nonEmpty)
      Some(OS(str, PLAIN))
    else
      None
  }

  addExtractor("userAdvertisingId") { request =>
    request.withOptionalFieldValue("userAdvertisingId")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("model") { request =>
    val manu = request
      .getOptionalField[JsString]("manufacturer")
      .getOrElse(JsString(""))
      .value
    val model =
      request.getOptionalField[JsString]("model").getOrElse(JsString("")).value
    val str = manu + model
    if (str.nonEmpty)
      Some(Model(manu + model, PLAIN))
    else
      None
  }

  addExtractor("locale") { request =>
    request.withOptionalFieldValue("locale")(value => Language(value, PLAIN))
  }

  addExtractor("width") { request =>
    request.withOptionalFieldValue("width")(value => Width(value, PLAIN))
  }

  addExtractor("height") { request =>
    request.withOptionalFieldValue("height")(value => Height(value, PLAIN))
  }

  addExtractor("roaming") { request =>
    request.withOptionalFieldValue("roaming")(value => Roaming(value, PLAIN))
  }

  addExtractor("uptime") { request =>
    request.withOptionalFieldValue("timeSinceBoot")(value =>
      Uptime(value, PLAIN))
  }

  addExtractor("rooted") { request =>
    request.withOptionalFieldValue("root")(value => Rooted(value, PLAIN))
  }

  addExtractor("orientation") { request =>
    request.withOptionalFieldValue("orientation")(value =>
      Orientation(value, PLAIN))
  }

  addExtractor("carrier") { request =>
    request.withOptionalFieldValue("ispName", "ispCarrierIdName")(value =>
      Carrier(value, PLAIN))
  }

  addMultiExtractor("ram") { request =>
    val ramUsed = request.getOptionalField[JsString]("usedRam") match {
      case Some(value) => value.value.toInt
      case None        => 0
    }
    val ramFree = request.getOptionalField[JsString]("freeRam") match {
      case Some(value) => value.value.toInt
      case None        => 0
    }
    (ramUsed, ramFree) match {
      case (0, 0) => List()
      case (0, y) => List(Some(RamFree(y.toString, PLAIN)))
      case (_, 0) => List()
      case (x, y) => List(Some(RamTotal((x + y).toString, PLAIN)))
    }
  }

  addExtractor("grid") { request =>
    request.withOptionalFieldValue("grid")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("cellSignalLevel") { request =>
    request.withOptionalFieldValue("cellSignalLevel")(value =>
      SignalStrengthCellular(value, PLAIN))
  }

  addExtractor("wifiSignalLevel") { request =>
    request.withOptionalFieldValue("wifiSignalLevel")(value =>
      SignalStrengthWifi(value, PLAIN))
  }

}
