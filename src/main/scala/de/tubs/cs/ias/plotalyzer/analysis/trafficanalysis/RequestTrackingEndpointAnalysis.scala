package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis,
  MobileApp
}
import de.tubs.cs.ias.plotalyzer.utility.AsciiProgressBar
import spray.json.{JsArray, JsNull, JsNumber, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

class RequestTrackingEndpointAnalysis(
    interfaceAnalysis: List[InterfaceAnalysis],
    conf: TrafficCollectionAnalysisConfig)
    extends LogSupport {

  private val appMap: Map[MobileApp, List[InterfaceAnalysis]] =
    interfaceAnalysis
      .groupBy(_.getApp)
      .filterNot(_._2.exists(_.getErrors.nonEmpty))
      .filter(_._2.nonEmpty)
  private val overallSize: Int = appMap.size
  info(
    s"we have analysis sets for $overallSize different apps, performing traffic analysis...")
  private val analysis
    : Map[MobileApp,
          List[(String, Option[AppRequestTrackingEndpointAnalysis])]] = {
    val bar = AsciiProgressBar.create("app analysis sets", overallSize.toLong)
    try {
      val fut = Future.sequence {
        appMap.map {
          case (app, analysis) =>
            Future {
              try {
                app -> conf.getAnalysisRules.map(rule => rule.prepare(analysis))
              } finally {
                bar.synchronized {
                  bar.step()
                }
              }
            }
        }
      }
      Await.result(fut, Inf).toMap
    } finally {
      bar.close()
    }
  }
  info("analysis done")

  def getAppRequestAnalysis: List[AppRequestTrackingEndpointAnalysis] =
    analysis.flatMap(_._2.map(_._2)).filter(_.nonEmpty).map(_.get).toList

  def getAppRequestAnalysisMap
    : Map[String, List[AppRequestTrackingEndpointAnalysis]] = {
    val ret: MMap[String, ListBuffer[AppRequestTrackingEndpointAnalysis]] =
      MMap()
    analysis.foreach { elem =>
      elem._2.foreach {
        case (str, Some(analysis)) =>
          ret.get(str) match {
            case Some(value) =>
              value.addOne(analysis)
            case None =>
              ret.addOne(str -> ListBuffer(analysis))
          }
        case (_, None) =>
      }
    }
    ret.map(elem => elem._1 -> elem._2.toList).toMap
  }

  def getOverallRequests: Int =
    getAppRequestAnalysis.map(_.getRequestCount).sum

  def getOverallInterestingRequests: Int =
    getAppRequestAnalysis.map(_.getInterestingRequests.length).sum

  def getCoveredByEndpoint: Int =
    getAppRequestAnalysis.map(_.getEndpointParserCovered).sum

  def getUnaddressed: Map[String, Map[String, Int]] = {
    val ret: MMap[String, MMap[String, Int]] = MMap()
    getAppRequestAnalysis.foreach { ana =>
      ana.getUnaddressedEndpoints.foreach {
        case (host, paths) =>
          ret.get(host) match {
            case Some(pathMap) =>
              paths.foreach {
                case (path, count) =>
                  pathMap.get(path) match {
                    case Some(value) => pathMap(path) = value + count
                    case None        => pathMap.addOne(path -> count)
                  }
              }
            case None =>
              val pathMap: MMap[String, Int] = MMap()
              paths.foreach {
                case (path, count) => pathMap.addOne(path -> count)
              }
              ret.addOne(host -> pathMap)
          }
      }
    }
    ret.map(elem => elem._1 -> elem._2.toMap).toMap
  }

  def getErrors: Map[String, List[String]] = {
    val ret: MMap[String, ListBuffer[String]] = MMap()
    getAppRequestAnalysis.foreach { ana =>
      ana.getErrors.foreach {
        case (endpoint, errors) =>
          ret.get(endpoint) match {
            case Some(value) => value.addAll(errors.map(_.value))
            case None =>
              ret.addOne(endpoint -> ListBuffer(errors.map(_.value): _*))
          }
      }
    }
    ret.map(elem => elem._1 -> elem._2.toList).toMap
  }

  /** generates a JsObject that can be pretty printed as specified below
    *
    * {
    *    "_requests" : <NUMBER>,
    *     "_interesting_requests" : <NUMBER>,
    *     "_requests_covered:" : <NUMBER>,
    *    "_analysis" : <NUMBER>,
    *    "pii" : {
    *      <PII> : {
    *         "anonymously" : <NUMBER>,
    *         "pseudonymously" : <NUMBER>
    *      }
    *    }
    *    "endpoints" : {
    *      <company> : {
    *        <PII> : {
    *          "anonymously" : <NUMBER>,
    *          "pseudonymously" : <NUMBER>
    *        }
    *      }
    *    }
    * }
    *
    * @param analysis the list of performed analysis to be sumamrized
    * @return the JsObject representing the summary
    */
  def generateAnalysisSummary(
      analysis: List[AppRequestTrackingEndpointAnalysis]): JsObject = {
    val requests = analysis.map(_.getRequestCount).sum
    val interestingRequests = analysis.map(_.getInterestingRequests.length).sum
    val requestsCovered = analysis.map(_.getEndpointParserCovered).sum
    val piiSummary: MMap[String, (Int, Int)] = MMap()
    val endpointSummary: MMap[String, MMap[String, (Int, Int)]] = MMap()
    analysis.foreach { ana =>
      ana.getPiiDistribution.foreach {
        case (pii, (anonCount, pseudoCount)) =>
          piiSummary.get(pii) match {
            case Some((sumanon, sumpseudo)) =>
              piiSummary(pii) = (sumanon + anonCount, sumpseudo + pseudoCount)
            case None =>
              piiSummary.addOne(pii -> (anonCount, pseudoCount))
          }
      }
      ana.getCompanyPiiDistribution.foreach {
        case (company, piiMap) =>
          if (!endpointSummary.contains(company))
            endpointSummary.addOne(company -> MMap())
          val endpointMap = endpointSummary(company)
          piiMap.foreach {
            case (pii, (anon, pseudo)) =>
              endpointMap.get(pii) match {
                case Some((anonSum, pseudoSum)) =>
                  endpointMap(pii) = (anonSum + anon, pseudoSum + pseudo)
                case None => endpointMap.addOne(pii -> (anon, pseudo))
              }
          }
      }
    }
    JsObject(
      "_requests" -> JsNumber(requests),
      "_interesting_requests" -> JsNumber(interestingRequests),
      "_requests_covered" -> JsNumber(requestsCovered),
      "_analysis" -> JsNumber(analysis.length),
      "pii" -> JsObject(piiSummary.map {
        case (pii, (anon, pseudo)) =>
          pii -> JsObject(
            "anonymous" -> JsNumber(anon),
            "pseudonymous" -> JsNumber(pseudo)
          )
      }.toMap),
      "endpoints" -> JsObject(
        endpointSummary.map {
          case (company, pii) =>
            company -> JsObject {
              pii.map {
                case (pii, (anon, pseudo)) =>
                  pii -> JsObject(
                    "sum" -> JsNumber(anon + pseudo),
                    "anonymous" -> JsNumber(anon),
                    "pseudonymous" -> JsNumber(pseudo)
                  )
              }.toMap
            }
        }.toMap
      )
    )
  }

  /** returns a JsObject that can be pretty printed as specified below
    *
    * {
    *    _overallRequests : <NUMBER>,
    *    _addressedByEndpointParser : <NUMBER>,
    *    _summary : {
    *      <ANALYSIS_DESCR> : <ANALYSIS_SUMMARY>,...
    *    },
    *    _unaddressed : {
    *      <ENDPOINT> : <INT>,...
    *    },
    *    apps : {
    *      <app> : {
    *        <ANALYSIS_DESCR> : <ANALYSIS>,...
    *      }
    *    },
    *    zerrors : {
    *       <ENDPOINT> : [<STR>,...],...
    *    }
    * }
    *
    * @return
    */
  def toJson: JsValue = {
    JsObject(
      "_overallRequests" -> JsNumber(this.getOverallRequests),
      "_overallInterestingRequests" -> JsNumber(
        this.getOverallInterestingRequests),
      "_addressedByEndpointParser" -> JsNumber(this.getCoveredByEndpoint),
      "_summary" -> JsObject(
        getAppRequestAnalysisMap.map {
          case (descr, analysis) =>
            descr -> generateAnalysisSummary(analysis)
        }
      ),
      "_unaddressed" -> AppRequestTrackingEndpointAnalysis
        .missedEndpointsToJsObject(this.getUnaddressed),
      "zerrors" -> JsObject(this.getErrors.filter(_._2.nonEmpty).map {
        case (endpoint, errors) =>
          endpoint -> JsArray(errors.map(value => JsString(value)).toVector)
      }),
      "apps" -> JsObject(
        this.analysis.map {
          case (app, analysis) =>
            app.toString -> JsObject(
              analysis.map {
                case (descr, analysis) =>
                  descr -> (analysis match {
                    case Some(value) => value.toJson
                    case None        => JsNull
                  })
              }.toMap
            )
        }
      )
    )
  }

}

object RequestTrackingEndpointAnalysis {

  def apply(experiment: Experiment,
            conf: TrafficCollectionAnalysisConfig,
            only: Set[String])(
      implicit database: Database): RequestTrackingEndpointAnalysis = {
    val analysis = InterfaceAnalysis
      .get(experiment)
      .filter(ana => only.isEmpty || only.contains(ana.getApp.toString))
    new RequestTrackingEndpointAnalysis(
      analysis,
      conf
    )
  }

}
