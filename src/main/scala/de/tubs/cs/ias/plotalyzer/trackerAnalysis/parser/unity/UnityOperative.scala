package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{EndpointParser, OS, UUID}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import spray.json.{JsObject, JsString}

import scala.util.matching.Regex

object UnityOperative extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://events.mz.unity3d.com/operative/.*".r
  )
  override val trackerName: String = "Unity3d [Operative]"
  override val trackingCompany: String = "Unity3d"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(_) => parseQueryFromBody(request)
      case None    => None
    }
  }

  addExtractor("platform") {
    _.withOptionalFieldValue("platform")(value => OS(value, PLAIN))
  }

  addExtractor("auctionId") { request =>
    request.getOptionalField[JsString]("auctionId") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None =>
        None
    }
  }

  addExtractor("gameSessionId") { request =>
    request.getOptionalField[JsString]("gameSessionId") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None => None
    }
  }
}
