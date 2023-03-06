package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{
  EndpointParser,
  OS,
  SDKVersion,
  UUID
}
import spray.json.{JsObject, JsString, JsonParser}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject

import scala.util.matching.Regex

object UnityState extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://publisher-config.unityads.unity3d.com/privacy/[0-9]+/state".r,
    "https://configv2.unityads.unity3d.com/privacy/[0-9]+/state".r
  )
  override val trackerName: String = "Unity3d [State]"
  override val trackingCompany: String = "Unity3d"

  override protected def prepare(request: Request): Option[JsObject] = {
    Option(request.content) match {
      case Some(value) => Some(JsonParser(value).asJsObject)
      case None        => None
    }
  }

  addExtractor("platform") {
    _.withOptionalFieldValue("platform")(value => OS(value, PLAIN))
  }

  addExtractor("idfi") { request =>
    request.getOptionalField[JsString]("idfi") match {
      case Some(value) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case None => None
    }
  }

  addExtractor("sdk") {
    _.withOptionalFieldValue("sdkVersionName")(value =>
      SDKVersion(value, PLAIN))
  }
}
