package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.TrafficCollectionAnalysisConfig
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.EndpointParser
import spray.json.{JsArray, JsNull, JsObject, JsString}

import java.util.Base64
import scala.annotation.nowarn
import scala.util.matching.Regex

class GenericRequestParser(@nowarn conf: TrafficCollectionAnalysisConfig)
    extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    ".*".r
  )

  override val trackerName: String = "generic"
  override val trackingCompany: String = "Generic"

  override protected def prepare(request: Request): Option[JsObject] = {
    Some(
      JsObject(
        "path" -> JsString(request.path),
        "body" -> JsString(request.content),
        "bodyRaw" -> (Option(request.contentRaw) match {
          case Some(value) => JsString(Base64.getEncoder.encodeToString(value))
          case None        => JsNull
        }),
        "cookies" -> JsArray(
          request.cookies
            .map(
              cookie =>
                JsObject(
                  "name" -> JsString(cookie.name),
                  "value" -> JsString(cookie.values)
              ))
            .toVector),
        "header" -> JsArray(
          request.headers
            .map(
              header =>
                JsObject(
                  "name" -> JsString(header.name),
                  "value" -> JsString(header.values)
              ))
            .toVector)
      ))
  }
}
