package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsArray, JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Bugsnag extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://sessions.bugsnag.com".r
  )
  override val trackerName: String = "Bugsnag"
  override val trackingCompany: String = "Bugsnag"
  override val requiredMethods: Option[List[String]] = Some(List("POST"))

  override protected def prepare(request: Request): Option[JsObject] =
    Some(JsonParser(request.content).asJsObject)

  addExtractor("notifier?.sdkversion") { request =>
    request.withOptionalField[JsObject]("notifier") { notifier =>
      SDKVersion(notifier.getField[JsString]("version").value, PLAIN)
    }
  }

  addExtractor("app?.version") { request =>
    request.withOptionalField[JsObject]("app") { app =>
      app.withFieldValue("version")(value => AppVersion(value, PLAIN))
    }
  }

  addExtractor("app?.id") { request =>
    request.getOptionalField[JsObject]("app") match {
      case Some(app) =>
        app.withOptionalFieldValue("id", "packageName")(value =>
          AppVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("app?.foreground") { request =>
    request.getOptionalField[JsObject]("app") match {
      case Some(app) =>
        app.withOptionalFieldValue("inForeground")(value =>
          InForeground(value, PLAIN))
      case None => None
    }
  }

  addExtractor("app?.activeScreen") { request =>
    request.getOptionalField[JsObject]("app") match {
      case Some(app) =>
        app.withOptionalFieldValue("activeScreen")(value =>
          CurrentlyViewed(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.architecture") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalField[JsArray]("cpuAbi") { arr =>
          val elems = arr.elements
            .map(_.asInstanceOf[JsString])
            .map(_.value)
            .mkString(" ")
          Architecture(elems, PLAIN)
        }
      case None => None
    }
  }

  addExtractor("device?.os") { request =>
    request.withOptionalField[JsObject]("device") { device =>
      OS(device.getField[JsString]("osName").value + device.getField[JsString](
           "osVersion"),
         PLAIN)
    }
  }

  addExtractor("device?.rooted") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("jailbroken")(value =>
          Rooted(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.model") { request =>
    request.withOptionalField[JsObject]("device") { device =>
      val brandString = device.getField[JsString]("manufacturer", "brand").value
      val modelString = device.getField[JsString]("model").value
      Model(brandString + modelString, PLAIN)
    }
  }

  addExtractor("device?.language") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.getOptionalField[JsString]("locale") match {
          case Some(locale) =>
            Some(Language(locale.value.split("_").head, PLAIN))
          case None => None
        }
      case None => None
    }
  }

  addExtractor("device?.user_agent") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("userAgent")(value =>
          UserAgent(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.ramTotal") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("totalMemory")(value =>
          RamTotal(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.timezone") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("timezone")(value =>
          TimeZone(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.charging") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("charging")(value =>
          IsCharging(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.diskFree") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("freeDisk")(value =>
          DiskFree(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.network") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("networkAccess")(value =>
          NetworkConnectionType(value, PLAIN))
      case None => None
    }
  }

  addExtractor("device?.emulator") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("emulator")(value =>
          Emulator(value, PLAIN))
      case None => None
    }
  }

  addMultiExtractor("device?.dimensions") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.getOptionalField[JsString]("screenResolution") match {
          case Some(resolution) =>
            List(
              Some(Height(resolution.value.split("x").head, PLAIN)),
              Some(Width(resolution.value.split("x").tail.head, PLAIN))
            )
          case None => List()
        }
      case None => List()
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

  addExtractor("device?.battery") { request =>
    request.getOptionalField[JsObject]("device") match {
      case Some(device) =>
        device.withOptionalFieldValue("batteryLevel")(value =>
          BatteryPercentage(value, PLAIN))
      case None => None
    }
  }
}
