package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.TrafficCollectionAnalysisConfig
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.adcolony._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.facebook.{
  FacebookGraph,
  FacebookGraphV
}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.vungle._
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}

object Dispatcher {

  private val endpointParser: List[EndpointParser] = List(
    Experimental,
    Adjust,
    UnityState,
    UnityOperative,
    UnityWebview,
    UnityGames,
    UnityEvents,
    Adc3Launch,
    OsNameAds,
    DeviceProvisioning,
    FirebaseRemoteConfig,
    FirebaseFcmToken,
    AppMeasurement,
    Register,
    Vungle1,
    Vungle2,
    Apple,
    Branchio,
    Bugsnag,
    Chartboost,
    Doubleclick,
    Ioam,
    Ironsource,
    Mopub,
    MsAppCenter,
    OneSignal,
    Rayjump,
    Startio,
    Supersonic,
    Yandex,
    FacebookGraph,
    FacebookGraphV,
    KnownEmptyRequests // this ensures we acknowledge requests of interest that do not contain any PII but still leak IP
  )

  def getEndpointCompany(request: Request): Option[String] = {
    endpointParser.find(_.requestMatchesEndpoint(request)) match {
      case Some(value) => Some(value.trackingCompany)
      case None        => None
    }
  }

  /** extracts contained PII based on our endpoint parsers
    *
    * @param requests the requests to be processed
    * @return the mapping of request to PII list as well as the list of remaining unaddressed requests
    */
  def extractPIIByEndpoints(
      requests: List[Request]): (Map[Request, List[PII]], List[Request]) = {
    val reamaining: ListBuffer[Request] = new ListBuffer()
    val hits: MMap[Request, List[PII]] = MMap()
    requests.foreach { request =>
      val ret = endpointParser.flatMap(_.parse(request))
      if (ret.nonEmpty) {
        hits.addOne(request -> ret)
      } else {
        reamaining.addOne(request)
      }
    }
    (hits.toMap, reamaining.toList)
  }

  /** extracts contained PII based on the string indicators provided by the conf
    *
    * @param request the requests to be processed
    * @param conf the conf containing the string indicators
    * @return the mapping of request to PII list
    */
  def extractPIIByGenericParser(
      request: List[Request],
      conf: TrafficCollectionAnalysisConfig): Map[Request, List[PII]] = {
    val parser = new GenericRequestParser(conf)
    request.map { request =>
      request -> parser.parse(request)
    }.toMap
  }

}
