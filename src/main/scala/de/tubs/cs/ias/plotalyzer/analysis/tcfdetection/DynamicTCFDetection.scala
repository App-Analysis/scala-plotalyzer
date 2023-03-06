package de.tubs.cs.ias.plotalyzer.analysis.tcfdetection

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis,
  MobileApp
}
import spray.json.{JsObject, JsValue}
import wvlet.log.LogSupport

class DynamicTCFDetection(analysis: List[InterfaceAnalysis],
                          conf: TCFDetectionConf)
    extends LogSupport {

  val appAnalysis: Map[MobileApp, AppDynamicTCFDetection] = analysis
    .groupBy(_.getApp)
    .map(pair => pair._1 -> new AppDynamicTCFDetection(pair._2))

  /** generate JSON that pretty prints as specified below
    *
    * {
    *    <APPID> : <APPIABANALYSIS>,...
    * }
    *
    *
    * @return JsObject
    */
  def toJson: JsValue = {
    JsObject(
      appAnalysis.map {
        case (app, result) =>
          app.toString -> result.toJson
      }
    )
  }

}

object DynamicTCFDetection {

  def apply(experiment: Experiment,
            conf: TCFDetectionConf,
            only: Set[String] = Set())(
      implicit database: Database): DynamicTCFDetection = {
    val analysis = InterfaceAnalysis
      .get(experiment)
      .filter(ana => only.isEmpty || only.contains(ana.getApp.toString))
    new DynamicTCFDetection(analysis, conf)
  }

}
