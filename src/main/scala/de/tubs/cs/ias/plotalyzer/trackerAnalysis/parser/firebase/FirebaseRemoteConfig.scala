package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsonParser}

import scala.util.matching.Regex

object FirebaseRemoteConfig extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://firebaseremoteconfig.googleapis.com/v1/projects/[0-9]+/namespaces/.*".r,
    "https://firebaseinstallations.googleapis.com/v1/projects/.*".r
  )
  override val trackerName: String = "firebase [RemoteConfig]"
  override val trackingCompany: String = "Firebase"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(value) if value.nonEmpty =>
        Some(JsonParser(value).asJsObject)
      case _ => None
    }
  }

  addExtractor("appVersion") {
    _.withOptionalFieldValue("appVersion")(value => AppVersion(value, PLAIN))
  }

  addExtractor("timeZone") {
    _.withOptionalFieldValue("timeZone")(value => TimeZone(value, PLAIN))
  }

  addExtractor("packageName") {
    _.withOptionalFieldValue("packageName")(value => AppID(value, PLAIN))
  }

  addExtractor("fid") {
    _.withOptionalFieldValue("fid")(value => OtherIdentifier(value, PLAIN))
  }

  addExtractor("sdkVersion") {
    _.withOptionalFieldValue("sdkVersion")(value => SDKVersion(value, PLAIN))
  }
}
