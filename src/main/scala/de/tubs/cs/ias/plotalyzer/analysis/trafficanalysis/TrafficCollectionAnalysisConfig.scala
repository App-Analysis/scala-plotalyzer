package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis

import de.tubs.cs.ias.plotalyzer.database.entities.InterfaceAnalysis
import spray.json.{DefaultJsonProtocol, JsonParser, RootJsonFormat}
import wvlet.log.LogSupport

import scala.io.Source

trait AnalysisRule {

  def conf: TrafficCollectionAnalysisConfig
  def prepare(analysis: List[InterfaceAnalysis])
    : (String, Option[AppRequestTrackingEndpointAnalysis])
}

case class Regular(descr: String,
                   override val conf: TrafficCollectionAnalysisConfig)
    extends AnalysisRule {

  override def prepare(analysis: List[InterfaceAnalysis])
    : (String, Option[AppRequestTrackingEndpointAnalysis]) = {
    analysis.find(_.getDescription == descr) match {
      case Some(value) =>
        val initial = value.getTrafficCollection.flatMap(_.getRequests)
        val traffic = AppRequestTrackingEndpointAnalysis.maxTimespan(
          initial,
          conf.timespanSec)
        (descr, Some(new AppRequestTrackingEndpointAnalysis(traffic, conf)))
      case None =>
        (descr, None)
    }
  }

}

case class Minus(minuend: String,
                 subtrahend: String,
                 override val conf: TrafficCollectionAnalysisConfig)
    extends AnalysisRule
    with LogSupport {

  override def prepare(analysis: List[InterfaceAnalysis])
    : (String, Option[AppRequestTrackingEndpointAnalysis]) = {
    val operation = s"${this.minuend} - ${this.subtrahend}"
    val subtrAna = analysis.find(_.getDescription == subtrahend)
    val minuendAna = analysis.find(_.getDescription == minuend)
    val app = analysis.map(_.getApp).distinct
    (minuendAna, subtrAna) match {
      case (Some(minuendV), Some(subtrahendV)) =>
        val minuendTraffic =
          minuendV.getTrafficCollection.flatMap(_.getRequests)
        val minuendTrafficFilteredByTime = AppRequestTrackingEndpointAnalysis
          .maxTimespan(minuendTraffic, conf.timespanSec)
        val minuendAnalysis = new AppRequestTrackingEndpointAnalysis(
          minuendTrafficFilteredByTime,
          conf)
        val reqs = minuendAnalysis.getRequestCount
        val subtrahendTraffic =
          subtrahendV.getTrafficCollection.flatMap(_.getRequests)
        val subtrahendTrafficFilteredByTime = AppRequestTrackingEndpointAnalysis
          .maxTimespan(subtrahendTraffic, conf.timespanSec)
        val subtrahendAnalysis = new AppRequestTrackingEndpointAnalysis(
          subtrahendTrafficFilteredByTime,
          conf)

        val ret = (operation, Some(minuendAnalysis - subtrahendAnalysis))
        assert(ret._2.get.getRequestCount == reqs)
        ret
      case (Some(_), None) =>
        // this is not suspected as this implies an error during the whole analysis process
        error(s"${app.head} is missing the $subtrahend analysis")
        (operation, None)
      case (None, Some(_)) =>
        // this is to be expected as we do not always have a reject or accept button
        (operation, None)
      case _ => (operation, None)
    }
  }

}

/**
  *
  * @param relevantEndpointHosts all hosts that are considered a relevant endpoint for traffic analysis
  * @param analyze rules on what interface analysis to analyze, either the name, or a delta i.e., IfaceDescrA OPERAND+ IfaceDescrB
  * @param timespanSec the time in seconds which are considered part of the traffic analysis
  */
case class TrafficCollectionAnalysisConfig(relevantEndpointHosts: List[String],
                                           analyze: List[String],
                                           timespanSec: Long) {

  def getAnalysisRules: List[AnalysisRule] = {
    val ret: List[AnalysisRule] = analyze.map { rule =>
      rule.split("OPERAND").toList match {
        case noOperand :: Nil =>
          Regular(noOperand, this)
        case lhs :: rhs :: Nil =>
          val operand = rhs.substring(0, 1)
          val actualRhs = rhs.substring(1)
          operand match {
            case "-" =>
              Minus(lhs.trim, actualRhs.trim, this)
            case x =>
              throw new RuntimeException(s"unknown operand $x")
          }
        case x => throw new RuntimeException(s"unknown analysis rule $x")
      }
    }
    ret
  }
}

object TrafficCollectionAnalysisConfig extends DefaultJsonProtocol {

  implicit val trafficCollectionAnalysisConfig
    : RootJsonFormat[TrafficCollectionAnalysisConfig] =
    jsonFormat3(TrafficCollectionAnalysisConfig.apply)

  def get(path: String): TrafficCollectionAnalysisConfig = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.getLines().mkString("\n"))
        .convertTo[TrafficCollectionAnalysisConfig]
    } finally {
      source.close()
    }
  }

}
