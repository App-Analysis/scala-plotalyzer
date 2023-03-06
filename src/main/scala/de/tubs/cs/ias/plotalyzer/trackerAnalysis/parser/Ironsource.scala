package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.matching.Regex

object Ironsource extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://logs.ironsrc.mobi/logs".r
  )
  override val trackerName: String = "ironsource"
  override val trackingCompany: String = "IronSource"

  override protected def prepare(request: Request): Option[JsObject] = {
    parseQueryFromPath(request) match {
      case Some(value) =>
        val b64 = value.getField[JsString]("data").value
        Some(
          JsonParser(new String(Base64.getDecoder.decode(b64),
                                StandardCharsets.UTF_8)).asJsObject)
      case None =>
        warn("expected query element in path")
        None
    }
  }

  addExtractor("model") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        val manu = data
          .getOptionalField[JsString]("deviceoem")
          .getOrElse(JsString(""))
          .value
        val model = data
          .getOptionalField[JsString]("devicemodel")
          .getOrElse(JsString(""))
          .value
        val str = manu + model
        if (str.nonEmpty)
          Some(Model(str, PLAIN))
        else
          None
      case None => None
    }
  }

  addExtractor("os") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        val name = data
          .getOptionalField[JsString]("deviceos")
          .getOrElse(JsString(""))
          .value
        val version = data
          .getOptionalField[JsString]("deviceosversion")
          .getOrElse(JsString(""))
          .value
        val str = name + version
        if (str.nonEmpty)
          Some(OS(str, PLAIN))
        else
          None
      case None => None
    }
  }

  addExtractor("model") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        val os = data.getOptionalField[JsString]("deviceos")
        if (os.nonEmpty && os.get.value == "android") {
          data.withOptionalFieldValue("deviceid")(value =>
            GlobalOSAdvertisingIdentifier(value, PLAIN))
        } else {
          None
        }
      case None => None
    }
  }

  addExtractor("connectiontype") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        data.withOptionalFieldValue("connectiontype")(value =>
          NetworkConnectionType(value, PLAIN))
      case None => None
    }
  }

  addExtractor("bundleid") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        data.withOptionalFieldValue("bundleid")(value => AppID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("appversion") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        data.withOptionalFieldValue("appversion")(value =>
          AppVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("sdkversion") { request =>
    request.getOptionalField[JsObject]("data") match {
      case Some(data) =>
        data.withOptionalFieldValue("sdkversion")(value =>
          SDKVersion(value, PLAIN))
      case None => None
    }
  }
}
