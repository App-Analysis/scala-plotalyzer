package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.utility.json.DeepMerge
import spray.json.{JsObject, JsString, JsonParser}

import java.io.ByteArrayInputStream
import java.util.Base64
import java.util.zip.GZIPInputStream
import scala.util.matching.Regex

object Supersonic extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://outcome-ssp.supersonicads.com/mediation".r
  )
  override val trackerName: String = "supersonic"
  override val trackingCompany: String = "Supersonic"

  override protected def prepare(request: Request): Option[JsObject] = {
    val blob = if (request.content != null && request.content.startsWith("{")) {
      request.content
    } else if (request.content != null) {
      val body = request.content.replace("\n", "")
      val inputStream = new GZIPInputStream(
        new ByteArrayInputStream(Base64.getDecoder.decode(body)))
      scala.io.Source.fromInputStream(inputStream).mkString
    } else {
      "{}"
    }
    val query = parseQueryFromPath(request).get
    val json = JsonParser(blob).asJsObject
    if (json.fields.contains("table") && json.fields.contains("data")) {
      Some(
        DeepMerge.merge(
          JsonParser(json.getField[JsString]("data").value).asJsObject,
          query))
    } else {
      Some(DeepMerge.merge(json, query))
    }
  }

  addExtractor("appVersion") { request =>
    request.withOptionalFieldValue("appVersion")(value =>
      AppVersion(value, PLAIN))
  }

  addExtractor("bundleId") { request =>
    request.withOptionalFieldValue("bundleId")(value => AppID(value, PLAIN))
  }

  addExtractor("mobileCarrier") { request =>
    request.withOptionalFieldValue("mobileCarrier")(value =>
      Carrier(value, PLAIN))
  }

  addExtractor("tz") { request =>
    request.withOptionalFieldValue("tz")(value => TimeZone(value, PLAIN))
  }

  addExtractor("advertisingId") { request =>
    request.withOptionalFieldValue("advertisingId")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("eventSessionId") { request =>
    request.withOptionalFieldValue("eventSessionId")(value =>
      UUID(value, PLAIN))
  }

  addExtractor("idfv") { request =>
    request.withOptionalFieldValue("idfv")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }
  addExtractor("sessionId") { request =>
    request.getOptionalField[JsString]("sessionId") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("userId") { request =>
    request.getOptionalField[JsString]("userId") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  }

  addExtractor("battery") { request =>
    request.withOptionalFieldValue("battery")(value =>
      BatteryPercentage(value, PLAIN))
  }

  addExtractor("connectionType") { request =>
    request.withOptionalFieldValue("connectionType")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("internalFreeMemory") { request =>
    request.withOptionalFieldValue("internalFreeMemory")(value =>
      RamFree(value, PLAIN))
  }

  addExtractor("jb") { request =>
    request.withOptionalFieldValue("jb")(value => Rooted(value, PLAIN))
  }

  addExtractor("os") { request =>
    val deviceos = request.getOptionalField[JsString]("deviceOS") match {
      case Some(value) => value.value
      case None        => ""
    }
    val osVersion = request.getOptionalField[JsString]("osVersion") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = deviceos + osVersion
    if (str.nonEmpty) {
      Some(OS(str, PLAIN))
    } else {
      None
    }
  }

  addExtractor("model") { request =>
    val oem = request.getOptionalField[JsString]("deviceOEM") match {
      case Some(value) => value.value
      case None        => ""
    }
    val model = request.getOptionalField[JsString]("deviceModel") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = oem + model
    if (str.nonEmpty) {
      Some(Model(str, PLAIN))
    } else {
      None
    }
  }

  addExtractor("sdkVersion") { request =>
    request.withOptionalFieldValue("sdkVersion")(value =>
      SDKVersion(value, PLAIN))
  }

}
