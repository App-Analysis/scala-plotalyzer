package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.vungle

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Vungle2 extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://ads.api.vungle.com/config".r,
    "https://adx.ads.vungle.com/api/v5/ads".r,
    "https://api.vungle.com/api/v5/ads".r,
    "https://events.api.vungle.com/api/v5/cache_bust".r,
  )
  override val trackerName: String = "Vungle [2]"
  override val trackingCompany: String = "Vungle"

  override protected def prepare(request: Request): Option[JsObject] =
    Some(JsonParser(request.content).asJsObject)

  addExtractor("device?.model") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        val dev = device
          .getOptionalField[JsString]("device")
          .getOrElse(JsString(""))
          .value
        val model = device
          .getOptionalField[JsString]("model")
          .getOrElse(JsString(""))
          .value
        val str = dev + model
        if (str.nonEmpty)
          Some(Model(str, PLAIN))
        else
          None
      case _ => None
    }
  }

  addExtractor("device?.os") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        val name =
          device.getOptionalField[JsString]("os").getOrElse(JsString("")).value
        val version =
          device.getOptionalField[JsString]("osv").getOrElse(JsString("")).value
        val str = name + version
        if (str.nonEmpty)
          Some(OS(str, PLAIN))
        else
          None
      case None => None
    }
  }

  addExtractor("carrier") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("carrier")(value => Carrier(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.width") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("w")(value => Width(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.height") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("h")(value => Height(value, PLAIN))
      case None => None
    }
  }

  addExtractor("app?.bundle") { request =>
    request.getOptionalField[JsObject]("app") match {
      case Some(app) =>
        app.withOptionalFieldValue("bundle")(value => AppID(value, PLAIN))
      case None => None
    }
  }

  private def withOperatingSystem(request: JsObject)(
      func: JsObject => Option[PII]) = {
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.getOptionalField[JsString]("ifa") match {
          case Some(ifa) =>
            Some(GlobalOSAdvertisingIdentifier(ifa.value, PLAIN))
          case None =>
            device.getOptionalField[JsObject]("ext") match {
              case Some(ext) =>
                ext.getOptionalField[JsObject]("vungle") match {
                  case Some(vungle) =>
                    vungle.getOptionalField[JsObject]("ios", "android") match {
                      case Some(os) =>
                        func(os)
                      case None => None
                    }
                  case None => None
                }
              case None => None
            }
        }
      case None => None
    }
  }

  addExtractor(
    "device?.ifa || device?.ext?.vungle?.android?.gaid || device?.ext?.vungle?.ios?.idfa") {
    request: JsObject =>
      request.getOptionalField[JsObject]("device") match {
        case Some(device) =>
          device.getOptionalField[JsString]("ifa") match {
            case Some(value) =>
              Some(GlobalOSAdvertisingIdentifier(value.value, PLAIN))
            case None =>
              withOperatingSystem(request) { os =>
                os.withOptionalFieldValue("gaid", "idfa")(value =>
                  GlobalOSAdvertisingIdentifier(value, PLAIN))
              }
          }
        case None => None
      }
  }

  addExtractor("device?.ua") { request: JsObject =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("ua")(value => UserAgent(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.ext?.vungle?.ios?.idfv") { request =>
    withOperatingSystem(request) { os =>
      os.getOptionalField[JsString]("idfv") match {
        case Some(value) =>
          Some(LocalOSAdvertisingIdentifier(value.value, PLAIN))
        case None => None
      }
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.battery_level") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("battery_level")(value =>
        BatteryPercentage(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.battery_state") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("battery_state")(value =>
        BatteryState(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.connection_type") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("connection_type")(value =>
        NetworkConnectionType(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.language") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("language")(value => Language(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.time_zone") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("time_zone")(value => TimeZone(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.volume_level") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("volume_level")(value => Volume(value, PLAIN))
    }
  }

  addExtractor("device?.ext?.vungle?.<os>?.disk_free") { request =>
    withOperatingSystem(request) { os =>
      os.withOptionalFieldValue("storage_bytes_available")(value =>
        Volume(value, PLAIN))
    }
  }

  addExtractor("app?.version") { request =>
    request.getOptionalField[JsObject]("app") match {
      case Some(app) =>
        app.withOptionalFieldValue("ver")(value => AppVersion(value, PLAIN))
      case None => None
    }
  }
}
