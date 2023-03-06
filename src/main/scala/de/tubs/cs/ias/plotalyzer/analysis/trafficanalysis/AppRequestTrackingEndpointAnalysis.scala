package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis

import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.AppRequestTrackingEndpointAnalysis.{
  missedEndpointsToJsObject,
  piiIsError
}
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue}

import scala.collection.mutable.{ListBuffer, Map => MMap}

class AppRequestTrackingEndpointAnalysis(
    allRequests: List[Request],
    conf: TrafficCollectionAnalysisConfig) {

  private val requests = allRequests.filter(_.error.isEmpty) // we only want to analyze requests we were able to intercept

  def getRequestCount: Int = requests.length

  /** this is dangerous as it changes the object, bad style but convenient
    *
    * @param other the analysis to be subtracted
    * @return the current object minus the already observed PII
    */
  def -(other: AppRequestTrackingEndpointAnalysis)
    : AppRequestTrackingEndpointAnalysis = {
    other.getEndpointPii.foreach {
      case (endpoint, otherPiis) =>
        endpointPii.get(endpoint) match {
          case Some(piis) =>
            val otherPiiSet: Set[PII] = otherPiis.toSet
            val newLB = ListBuffer[PII]()
            piis
              .filterNot(elem => otherPiiSet.contains(elem))
              .foreach(newLB.addOne)
            endpointPii(endpoint) = newLB
          case None => // if we do not have this endpoint we do not need to subtract
        }
    }
    determinePiiDistribution()
    this
  }

  private val interestingRequests =
    AppRequestTrackingEndpointAnalysis.filterForRelevantEndpoints(
      requests,
      conf.relevantEndpointHosts.toSet)

  def getInterestingRequests: List[Request] = interestingRequests
  private val endpointPii: MMap[String, ListBuffer[PII]] = MMap()
  private val requestPii: MMap[String, ListBuffer[PII]] = MMap()
  def getEndpointPii: Map[String, List[PII]] =
    endpointPii
      .filter(_._2.nonEmpty)
      .map(elem => elem._1 -> elem._2.toList)
      .toMap
  private val errors: MMap[String, ListBuffer[PII]] = MMap()
  def getErrors: Map[String, List[PII]] =
    errors.map(elem => elem._1 -> elem._2.toList).toMap
  private def filterErrors(
      result: Map[Request, List[PII]]): Map[Request, List[PII]] = {
    result.map {
      case (request, piis) =>
        errors.get(request.host) match {
          case Some(value) => value.addAll(piis.filter(piiIsError))
          case None =>
            errors.addOne(
              request.host -> ListBuffer(piis.filter(piiIsError): _*))
        }
        request -> piis.filterNot(piiIsError)
    }
  }
  private val piiDistribution: MMap[String, (Int, Int)] = MMap()
  private val companyPiiDistribution: MMap[String, MMap[String, (Int, Int)]] =
    MMap()
  def getPiiDistribution: Map[String, (Int, Int)] = piiDistribution.toMap
  def getCompanyPiiDistribution: Map[String, Map[String, (Int, Int)]] =
    companyPiiDistribution.map(elem => elem._1 -> elem._2.toMap).toMap
  def addParsedPii(request: Request, pii: List[PII]): Unit = synchronized {
    requestPii.get(request.host) match {
      case Some(value) => value.addAll(pii)
      case None        => requestPii.addOne(request.host -> ListBuffer(pii: _*))
    }
    Dispatcher.getEndpointCompany(request) match {
      case Some(company) =>
        endpointPii.get(company) match {
          case Some(value) => value.addAll(pii)
          case None        => endpointPii.addOne(company -> ListBuffer(pii: _*))
        }
      case None =>
    }
  }
  private var endpointParserCovered: Int = 0
  def getEndpointParserCovered: Int = endpointParserCovered
  private val unaddressedEndpoints: MMap[String, MMap[String, Int]] = MMap()
  def addUnaddressedEndpoint(request: Request): Unit = {
    unaddressedEndpoints.get(request.host) match {
      case Some(host) =>
        host.get(request.getPath) match {
          case Some(value) => host(request.getPath) = value + 1
          case None        => host.addOne(request.getPath -> 1)
        }
      case None =>
        unaddressedEndpoints.addOne(
          request.host -> MMap(request.getPath -> 1)
        )
    }
  }
  def getUnaddressedEndpoints: Map[String, Map[String, Int]] =
    unaddressedEndpoints.map(elem => elem._1 -> elem._2.toMap).toMap

  private def determinePiiDistribution(): Unit = {
    piiDistribution.clear()
    endpointPii.foreach {
      case (company, pii) =>
        val arePseudo = {
          AppRequestTrackingEndpointAnalysis.piisArePseudonymous(pii.toSeq)
        }
        val (addAnon, addPseudo) = if (arePseudo) (0, 1) else (1, 0)
        // this here means that even if there are multiple same pii for the same company they wont counted multiple times
        pii.map(_.getPIIName).toSet.foreach { piiName: String =>
          piiDistribution.get(piiName) match {
            case Some((anon, pseudo)) =>
              piiDistribution.addOne(
                piiName -> (anon + addAnon, pseudo + addPseudo))
            case None =>
              piiDistribution.addOne(piiName -> (addAnon, addPseudo))
          }
          companyPiiDistribution.get(company) match {
            case Some(companyPii) =>
              companyPii.get(piiName) match {
                case Some((anon, pseudo)) =>
                  companyPii(piiName) = (anon + addAnon, pseudo + addPseudo)
                case None =>
                  companyPii.addOne(piiName -> (addAnon, addPseudo))
              }
            case None =>
              if (arePseudo)
                companyPiiDistribution.addOne(
                  company -> MMap(piiName -> (0, 1)))
              else
                companyPiiDistribution.addOne(
                  company -> MMap(piiName -> (1, 0)))
          }
        }
    }
  }

  {
    val (parsed, remaining) =
      Dispatcher.extractPIIByEndpoints(interestingRequests)
    filterErrors(parsed).foreach {
      case (req, pii) => addParsedPii(req, pii)
    }
    endpointParserCovered = parsed.size
    filterErrors(Dispatcher.extractPIIByGenericParser(remaining, conf))
      .foreach {
        case (req, pii) =>
          addParsedPii(req, pii)
          addUnaddressedEndpoint(req)
      }
    determinePiiDistribution()
  }

  /** returns a JsObject that can be pretty printed as specified below
    *
    * {
    *    "_summary" : {
    *        "_overall_requests:" <NUMBER>,
    *       "_interesting_requests" : <NUMBER>,
    *       "_requests_covered" : <NUMBER>,
    *        "unaddressed" : {
    *           <ENDPOINT> : {
    *               "_count" : <INT>,
    *               "paths" : {
    *                  <STR> : <INT>,...
    *                }
    *            }
    *        }
    *       "detectedPII" : {
    *          <PII> : {
    *             "pseudonymously" : <NUMBER>,
    *             "anonymously" : <NUMBER>
    *          },...
    *        }
    *    },
    *    "endpoints" : {
    *        <ENDPOINT> : {
    *              <pii> : [<STRINGS>,...]
    *        },...
    *     }
    *     z_errors : {
    *        <ENDPOINT> : [<STR>,...],...
    *     }
    * }
    *
    * @return the JsObject
    */
  def toJson: JsValue = {
    JsObject(
      "_summary" -> JsObject(
        "_overall_requests" -> JsNumber(this.requests.length),
        "_interesting_requests" -> JsNumber(this.interestingRequests.length),
        "_requests_covered" -> JsNumber(this.endpointParserCovered),
        "unaddressed" -> missedEndpointsToJsObject(
          this.getUnaddressedEndpoints),
        "detectedPii" -> JsObject(this.piiDistribution.map {
          case (pii, (anon, pseudo)) =>
            pii -> JsObject("anon" -> JsNumber(anon),
                            "pseudo" -> JsNumber(pseudo))
        }.toMap)
      ),
      "endpoints" -> JsObject(getEndpointPii.map {
        case (point, pii) =>
          point -> JsObject(pii.groupBy(_.getPIIName).map {
            case (name, vals) =>
              name -> JsArray(
                vals
                  .map(_.value)
                  .toSet
                  .map((str: String) => JsString(str))
                  .toVector)
          })
      })
    )
  }

}

object AppRequestTrackingEndpointAnalysis {

  def missedEndpointsToJsObject(
      missed: Map[String, Map[String, Int]]): JsObject = {
    JsObject(
      missed.map {
        case (host, pathMap) =>
          val sum = pathMap.values.sum
          host -> JsObject(
            "_sum" -> JsNumber(sum),
            "path" -> JsObject(
              pathMap.map {
                case (path, count) => path -> JsNumber(count)
              }
            )
          )
      }
    )
  }

  def piisArePseudonymous(piis: Seq[PII]): Boolean =
    piis.exists(piiIsIdentifier)

  def piiIsError(pii: PII): Boolean = pii match {
    case _: RequestParsingIssue  => true
    case _: UnexpectedValue      => true
    case _: MissingExpectedValue => true
    case _: ExtractorFailure     => true
    case _                       => false
  }

  def piiIsIdentifier(pii: PII): Boolean = pii match {
    case _: GlobalOSAdvertisingIdentifier => true
    case _: LocalOSAdvertisingIdentifier  => true
    case _: UUID                          => true
    case _                                => false
  }

  def filterForRelevantEndpoints(requests: List[Request],
                                 endpointHosts: Set[String]): List[Request] = {
    requests.filter(
      req =>
        req.error.isEmpty && // for now we only want to analyze requests we actually were able to intercept
          endpointHosts.exists { host: String =>
            host.r.unanchored.matches(req.getHost)
        })
  }

  def maxTimespan(req: List[Request], timespanSeconds: Long): List[Request] = {
    implicit class RequestTimeOrder(req: Request) extends Ordered[Request] {

      override def compare(that: Request): Int =
        this.req.start.toEpochSecond.compare(that.start.toEpochSecond)
    }

    if (req.nonEmpty) {
      val first = req.min
      req.filter(req =>
        req.start.toEpochSecond - first.start.toEpochSecond <= timespanSeconds)
    } else {
      List()
    }
  }

}
