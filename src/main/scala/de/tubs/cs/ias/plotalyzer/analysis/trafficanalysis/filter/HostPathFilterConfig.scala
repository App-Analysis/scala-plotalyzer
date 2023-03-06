package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter

import spray.json.{DefaultJsonProtocol, JsonParser, RootJsonFormat}

import scala.io.Source

sealed trait HostPathRequestFilter {

  def getFilterRulesOnly: List[HostPathFilterElement]

}

case class HostPathFilterElement(host: String, path: String)

case class HostPathFilterConfig(
    filter: Map[String, List[HostPathFilterElement]])
    extends HostPathRequestFilter {

  def getFilterRulesOnly: List[HostPathFilterElement] =
    filter.values.flatten.toList

}

case class ExodusTrackerElement(id: Int,
                                name: String,
                                description: String,
                                creation_date: String,
                                code_signature: String,
                                network_signature: String,
                                website: String,
                                categories: List[String],
                                documentation: String) {

  def convertToHostPathFilterElement(): HostPathFilterElement = {
    HostPathFilterElement(network_signature, ".*")
  }
}

case class ExodusTrackerList(trackers: Map[String, ExodusTrackerElement])
    extends HostPathRequestFilter {

  def getFilterRulesOnly: List[HostPathFilterElement] =
    trackers.values.map(_.convertToHostPathFilterElement()).toList

  def convertToHostPathFilterConfig(): HostPathFilterConfig = {
    HostPathFilterConfig(
      trackers.map(elem =>
        elem._2.name -> List(elem._2.convertToHostPathFilterElement()))
    )
  }

}

object HostPathFilterTypes extends Enumeration {
  type HostPathFilterType = Value
  val EXODUS, GENERIC = Value
}

object HostPathFilterConfig extends DefaultJsonProtocol {

  implicit val hostPathFilterElementFormat
    : RootJsonFormat[HostPathFilterElement] = jsonFormat2(HostPathFilterElement)

  implicit val hostPathFilterConfigFormat
    : RootJsonFormat[HostPathFilterConfig] = jsonFormat1(
    HostPathFilterConfig.apply)

  implicit val exodusTrackerElementFormat
    : RootJsonFormat[ExodusTrackerElement] = jsonFormat9(ExodusTrackerElement)

  implicit val exodusTrackerListFormat: RootJsonFormat[ExodusTrackerList] =
    jsonFormat1(ExodusTrackerList)

  def readFilterList(path: String): HostPathFilterConfig = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.mkString).convertTo[HostPathFilterConfig]
    } finally {
      source.close()
    }
  }

  def readExodusList(path: String): ExodusTrackerList = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.mkString).convertTo[ExodusTrackerList]
    } finally {
      source.close()
    }
  }

}
