package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter

import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.HostPathFilterTypes.{
  EXODUS,
  HostPathFilterType,
  GENERIC
}
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request

trait TrafficFilter {

  def filter(requests: List[Request]): List[Request]

}

object TrafficFilter {

  def getHostPathFilter(filterJson: String,
                        filterType: HostPathFilterType): TrafficFilter = {
    filterType match {
      case EXODUS =>
        HostPathTrafficFilter(HostPathFilterConfig.readExodusList(filterJson))
      case GENERIC =>
        HostPathTrafficFilter(HostPathFilterConfig.readFilterList(filterJson))
    }
  }

  def getTimeFilter(timeSpanSec: Long): TimeSpanFilter = {
    TimeSpanFilter(timeSpanSec)
  }

}
