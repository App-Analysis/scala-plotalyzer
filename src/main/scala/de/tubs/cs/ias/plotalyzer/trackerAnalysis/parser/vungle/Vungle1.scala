package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.vungle

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.JsObject

import scala.util.matching.Regex

object Vungle1 extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://api.vungle.com/api/v\\d/new/".r
  )
  override val trackerName: String = "vungle [1]"
  override val trackingCompany: String = "Vungle"

  override protected def prepare(request: Request): Option[JsObject] = {
    parseQueryFromPath(request)
  }

  addExtractor("ifa") { request =>
    request.withOptionalFieldValue("ifa")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }
}
