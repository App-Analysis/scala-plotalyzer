package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request

case class HostPathTrafficFilter(conf: HostPathRequestFilter)
    extends TrafficFilter {

  override def filter(requests: List[Request]): List[Request] = {
    requests.filter { request =>
      conf.getFilterRulesOnly.contains { rule: HostPathFilterElement =>
        rule.host.r.matches(request.host) &&
        rule.path.r.matches(request.getPath)
      }
    }
  }

}
