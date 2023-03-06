package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{
  AppID,
  AppVersion,
  EndpointParser,
  OS,
  OtherIdentifier
}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.JsObject

import scala.util.matching.Regex

object FirebaseFcmToken extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://fcmtoken.googleapis.com/register".r
  )
  override val trackerName: String = "firebase [FcmToken]"
  override val trackingCompany: String = "Firebase"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(_) => parseQueryFromBody(request)
      case None    => None
    }
  }

  addExtractor("X-osv") {
    _.withOptionalFieldValue("X-osv")(value => OS(value, PLAIN))
  }

  addExtractor("device") {
    _.withOptionalFieldValue("device")(value => OtherIdentifier(value, PLAIN))
  }

  addExtractor("app") {
    _.withOptionalFieldValue("app")(value => AppID(value, PLAIN))
  }

  addExtractor("app_ver") {
    _.withOptionalFieldValue("app_ver")(value => AppVersion(value, PLAIN))
  }
}
