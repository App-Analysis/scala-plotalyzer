package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString}

import scala.util.matching.Regex

object Register extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://fcmtoken.googleapis.com/register".r
  )
  override val trackerName: String = "firebase [register]"
  override val trackingCompany: String = "Firebase"

  override protected def prepare(request: Request): Option[JsObject] = {
    parseQueryFromBody(request)
  }

  addExtractor("os") { request =>
    if (request.getField[JsString]("plat").value == "2") {
      Some(OS("iOS", PLAIN))
    } else {
      None
    }
  }

  addExtractor("device") { request =>
    request.getOptionalField[JsString]("device") match {
      case Some(value) => UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None        => None
    }
  }

  addExtractor("app") { request =>
    request.withOptionalFieldValue("app")(value => AppID(value, PLAIN))
  }

  addExtractor("app_ver") { request =>
    request.withOptionalFieldValue("app_ver")(value => AppVersion(value, PLAIN))
  }
}
