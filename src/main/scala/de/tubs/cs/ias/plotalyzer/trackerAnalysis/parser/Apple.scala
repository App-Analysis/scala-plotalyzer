package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsonParser}

import scala.util.matching.Regex

object Apple extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://ca.iadsdk.apple.com/adserver/attribution/v2".r
  )
  override val trackerName: String = "apple"
  override val trackingCompany: String = "Apple"

  override protected def prepare(request: Request): Option[JsObject] = {
    Some(JsonParser(request.content).asJsObject)
  }

  addExtractor("toroid") { request =>
    request.withOptionalFieldValue("toroId")(value => UUID(value, PLAIN))
  }

  addExtractor("anonymousDemandId") { request =>
    request.withOptionalFieldValue("anonymousDemandId")(value =>
      UUID(value, PLAIN))
  }

  addExtractor("bundleId") { request =>
    request.withOptionalFieldValue("bundleId")(value => AppID(value, PLAIN))
  }
}
