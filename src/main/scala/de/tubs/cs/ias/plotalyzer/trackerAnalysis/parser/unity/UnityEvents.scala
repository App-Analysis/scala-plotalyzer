package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object UnityEvents extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://cdp.cloud.unity3d.com/v1/events".r,
    "https://config.uca.cloud.unity3d.com/".r,
    "https://httpkafka.unityads.unity3d.com/v1/events".r, //todo: missing 36
    "https://thind.unityads.unity3d.com/v1/events".r,
  )
  override val trackerName: String = "unity [events]"
  override val trackingCompany: String = "Unity3d"

  override protected def prepare(request: Request): Option[JsObject] = {
    // The bodies hold multiple events. We only support the first one, which is identified by the `common` property.
    request.content
      .split("\n")
      .toList
      .filter(_.nonEmpty)
      .map { string =>
        try {
          Some(JsonParser(string).asJsObject)
        } catch {
          case _: Throwable => None
        }
      }
      .filter(_.nonEmpty)
      .map(_.get)
      .find { obj =>
        obj.fields.contains("common")
      } match {
      case Some(value) => Some(value.fields("common").asJsObject)
      case None        => None
    }
  }

  addExtractor("appid") { request =>
    request.getOptionalField[JsString]("storeId") match {
      case Some(value) => Some(AppID(value.value, PLAIN))
      case None =>
        request.getOptionalField[JsObject]("client") match {
          case Some(client) =>
            client.withOptionalFieldValue("bundleId")(value =>
              AppID(value, PLAIN))
          case None => None
        }
    }
  }

  addExtractor("bundleVersion") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.withOptionalFieldValue("bundleVersion")(value =>
          AppVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("sdkversion") { request =>
    request.getOptionalField[JsString]("adsSdkVersion") match {
      case Some(value) =>
        Some(SDKVersion(value.value, PLAIN))
      case None =>
        val ver = request.getOptionalField[JsString]("sdk_ver")
        val rev = request.getOptionalField[JsString]("sdk_rev")
        (ver, rev) match {
          case (None, None) => None
          case (x, y) =>
            Some(SDKVersion(
              x.getOrElse(JsString("")).value + y.getOrElse(JsString("")).value,
              PLAIN))
        }
    }
  }

  addExtractor("userid") { request =>
    request.withOptionalFieldValue("userid")(value => UUID(value, PLAIN))
  }

  addExtractor("deviceid") { request =>
    request.withOptionalFieldValue("userid")(value => UUID(value, PLAIN))
  }

  addExtractor("device_id") { request =>
    request.withOptionalFieldValue("userid")(value => UUID(value, PLAIN))
  }

  addExtractor("analyticsUserId") { request =>
    request.withOptionalFieldValue("userid")(value => UUID(value, PLAIN))
  }

  addExtractor("model") { request =>
    val maker = request.getOptionalField[JsString]("deviceMake") match {
      case Some(value) => value.value
      case None =>
        request.getOptionalField[JsObject]("device") match {
          case Some(value) =>
            value
              .getOptionalField[JsString]("deviceMake")
              .getOrElse(JsString(""))
              .value
          case None => ""
        }
    }
    val model = request.getOptionalField[JsString]("deviceModel") match {
      case Some(value) => value.value
      case None =>
        request.getOptionalField[JsObject]("device") match {
          case Some(value) =>
            value
              .getOptionalField[JsString]("deviceModel")
              .getOrElse(JsString(""))
              .value
          case None => ""
        }
    }
    val full = maker + model
    if (full.nonEmpty) {
      Some(Model(full, PLAIN))
    } else {
      None
    }
  }

  addExtractor("device?.screenWidth") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("screenWidth")(value =>
          Width(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.screenHeight") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("screenHeight")(value =>
          Height(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.networkOperatorName") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("networkOperatorName")(value =>
          Carrier(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.connectionType") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("connectionType")(value =>
          NetworkConnectionType(value, PLAIN))
      case None =>
        request.withOptionalFieldValue("connectionType")(value =>
          NetworkConnectionType(value, PLAIN))
    }
  }

  addExtractor("device?.timeZone") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("timeZone")(value =>
          TimeZone(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.language") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("language")(value =>
          Language(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.deviceVolume") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("deviceVolume")(value =>
          Volume(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.freeSpaceInternal") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("freeSpaceInternal")(value =>
          DiskFree(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.totalSpaceInternal") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("totalSpaceInternal")(value =>
          DiskTotal(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.batteryLevel") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("batteryLevel")(value =>
          BatteryPercentage(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.freeMemory") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("freeMemory")(value =>
          RamFree(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.totalMemory") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("totalMemory")(value =>
          RamTotal(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.rooted") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("rooted")(value => Rooted(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.userAgent") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("userAgent")(value =>
          UserAgent(value, PLAIN))
      case None => None
    }
  }

  addExtractor("country") { request =>
    request.withOptionalFieldValue("country")(value => Country(value, PLAIN))
  }
}
