package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{EmptyRequest, EndpointParser}
import spray.json.{JsObject, JsString}

import scala.util.matching.Regex

/** This endpoint parser handles all the URLs we already analyzed in one of the specialized parser or know
  *  to not contain usable data. This ensures that we know how many (tracking) requests we can process/handle using
  *  our endpoint specific handler and also allows to focus on the remaining enpoints.
  *
  */
object KnownEmptyRequests extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://cdp.cloud.unity3d.com".r,
    "https: //config.uca.cloud.unity3d.com/.*json".r,
    "https://mediation-tracking.prd.mz.internal.unity3d.com/api/v1/.*".r,
    "https://mediation-tracking.unityads.unity3d.com/api/.*".r,
    "https://api.onesignal.com/apps/.*".r, // there could be a uuid but she is related to app not to user
    "https://googleads.g.doubleclick.net/favicon.ico".r,
    "https://googleads.g.doubleclick.net/pagead/.*".r,
    "https://googleads.g.doubleclick.net/mads/.*".r,
    "https://publisher-config.unityads.unity3d.com/games/[0-9]+/configuration".r, // those can be empty
    "https://configv2.unityads.unity3d.com/games/[0-9]+/configuration".r, // those can be empty
    "https://c4d-cdn.adcolony.com/libs/.*".r,
    "https://cdn-lb.vungle.com/assets/.*".r,
    "https://cdn-lb.vungle.com/templates/.*".r,
    "https://cdn-lb.vungle.com/template-rtb/.*".r,
    "https://cdn-lb.vungle.com/zen/.*".r,
    "https://init.supersonicads.com/sdk/.*".r,
    "https://events3.adcolony.com/t/.*".r,
    "https://c4d-cdn.adcolony.com/adc/.*".r,
    "https://storage.googleapis.com/epcloud-ots-prod/default/.*".r,
    "https://info.startappservice.com/InApp/resources/.*".r
  )
  override val trackerName: String = "Empty"
  override val trackingCompany: String = "Misc"

  override protected def prepare(request: Request): Option[JsObject] = {
    Some(
      JsObject(
        "url" -> JsString(request.host)
      ))
  }

  addExtractor("empty") {
    _.withOptionalFieldValue("url")(value => EmptyRequest(value))
  }
}
