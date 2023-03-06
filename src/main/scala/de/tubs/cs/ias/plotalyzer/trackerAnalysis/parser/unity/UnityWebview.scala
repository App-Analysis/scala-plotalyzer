package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsObject, JsString, JsonParser}
import scala.util.matching.Regex

object UnityWebview extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://configv2.unityads.unity3d.com/webview.*".r,
    "https://config.unityads.unity3d.com/webview.*".r
  )
  override val trackerName: String = "Unity3d [auction]"
  override val trackingCompany: String = "Unity3d"

  override protected def prepare(request: Request): Option[JsObject] = {
    val contentJson = Option(request.content) match {
      case Some(value) if value.nonEmpty => JsonParser(value).asJsObject
      case _                             => JsObject()
    }
    val urlJson = request.getPath.split('?').toList match {
      case Nil               => JsObject()
      case _ :: Nil          => JsObject()
      case _ :: query :: Nil => parseQueryString(query)
      case _ :: query        => parseQueryString(query.mkString("?"))
    }
    val ret = DeepMerge.merge(contentJson, urlJson)
    if (ret.fields.nonEmpty) {
      Some(ret)
    } else {
      None
    }
  }

  addExtractor("sdkVersion") {
    _.withOptionalFieldValue("skdVersionName", "skdVersion")(value =>
      SDKVersion(value, PLAIN))
  }

  addExtractor("language") {
    _.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  }

  addExtractor("connectionType") {
    _.withOptionalFieldValue("connectionType")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("deviceFreeSpace") {
    _.withOptionalFieldValue("deviceFreeSpace")(value => DiskFree(value, PLAIN))
  }

  addExtractor("networkOperator") {
    _.withOptionalFieldValue("networkOperator")(value => Carrier(value, PLAIN))
  }

  addExtractor("idfi") {
    _.withOptionalFieldValue("idfi")(value => UUID(value, PLAIN))
  }

  addExtractor("volume") {
    _.withOptionalFieldValue("volume")(value => Volume(value, PLAIN))
  }

  addExtractor("screenWidth") {
    _.withOptionalFieldValue("screenWidth")(value => Width(value, PLAIN))
  }

  addExtractor("platform") { request =>
    val os = request.getOptionalField[JsString]("platform") match {
      case Some(value) => value.value
      case None        => ""
    }
    if (os.nonEmpty)
      Some(OS(os, PLAIN))
    else
      None
  }

  addExtractor("trackingId") {
    _.withOptionalFieldValue("unifiedconfig.pii.advertisingTrackingId")(value =>
      UUID(value, PLAIN))
  }

  addExtractor("webviewUa") {
    _.withOptionalFieldValue("webviewUa")(value => UserAgent(value, PLAIN))
  }

  addExtractor("deviceModel") {
    _.withOptionalFieldValue("deviceModel")(value => Model(value, PLAIN))
  }

}
