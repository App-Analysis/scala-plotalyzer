package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis

import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.TrafficFilter
import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis
}
import spray.json.{JsNumber, JsObject, JsString, JsValue}

case class BasicRequest(scheme: String,
                        host: String,
                        port: String,
                        path: String) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case x: Request =>
        x.scheme == this.scheme && x.host == this.host && x.port == this.port && x.path == this.path
      case x: BasicRequest =>
        x.scheme == this.scheme && x.host == this.host && x.port == this.port && x.path == this.path
      case _ => false
    }
  }

}

object BasicRequest {

  def apply(request: Request): BasicRequest = {
    BasicRequest(request.scheme, request.host, request.port, request.path)
  }

}

class TrafficSummary(analysis: List[InterfaceAnalysis],
                     filter: List[TrafficFilter]) {

  private val requests: List[BasicRequest] =
    analysis
      .map(
        _.getTrafficCollection.flatMap(_.getRequests.filter(_.error.isEmpty)))
      .flatMap { requestSets =>
        filter.flatMap(filter => filter.filter(requestSets))
      }
      .map(BasicRequest.apply)

  def getRequests: List[BasicRequest] = requests

  case class HostRequestSummary(host: String,
                                count: Int,
                                paths: List[(String, Int)]) {

    def toJson: JsValue = {
      JsObject(
        "host" -> JsString(host),
        "count" -> JsNumber(count),
        "paths" -> JsObject(
          paths
            .sortBy(_._2)
            .map {
              case (path, count) =>
                count.toString -> JsString(path)
            }
            .toMap
        )
      )
    }

  }

  def getHostRequestList: List[HostRequestSummary] = {
    requests
      .groupBy(_.host)
      .map {
        case (host, requests) =>
          val count = requests.length
          val pathMap = requests
            .groupBy(_.path)
            .map { case (path, paths) => (path, paths.length) }
            .toList
          HostRequestSummary(host, count, pathMap)
      }
      .toList
      .sortBy(_.count)
  }

  def toJson: JsValue = {
    JsObject(
      "count" -> JsNumber(this.requests.length),
      "hosts" -> JsObject(getHostRequestList.map { elem =>
        elem.host -> elem.toJson
      }.toMap)
    )
  }

}

object TrafficSummary {

  def apply(experiment: Experiment,
            filter: List[TrafficFilter],
            only: Set[String])(implicit database: Database): TrafficSummary = {
    val analysis = InterfaceAnalysis
      .get(experiment)
      .filter(ana => only.isEmpty || only.contains(ana.getApp.toString))
    new TrafficSummary(analysis, filter)
  }

}
