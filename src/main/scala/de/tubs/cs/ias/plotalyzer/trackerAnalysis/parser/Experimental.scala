package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.EndpointParser
import spray.json.JsObject

import scala.util.matching.Regex

/** This class only exists as an helper to analyze new yet unknown (tracking) urls in our dataset
  *
  */
object Experimental extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    // "https://codepush.appcenter.ms/v0.1/.*".r, //78
    //"https://firebasestorage.googleapis.com/v0/.*".r //160
    //"https://googleads.g.doubleclick.net/getconfig/pubsetting".r
    //"https://googleads.g.doubleclick.net/mads/.*".r,
  )
  override val trackerName: String = "Experimental"
  override val trackingCompany: String = "Experimental"

  override protected def prepare(request: Request): Option[JsObject] = {
    println(request.getFullUrl)
    println(request.content)
    println(request.contentRaw.length)
    println(request.cookies)
    //println(request.headers)
    None
  }
}
