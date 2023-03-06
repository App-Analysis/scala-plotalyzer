package de.tubs.cs.ias.plotalyzer.analysis

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis
}
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue}

class FailedAnalysisSummary(analysis: List[InterfaceAnalysis]) {

  private val failedAnalysis = analysis.filter(_.getErrors.nonEmpty)
  private val failedApps = failedAnalysis.map(_.getApp).distinct

  /** generates a JsObject that pretty prints to the specs below
    *
    * {
    *    "_failed" : {
    *        "analysis" : <NUMBER>,
    *        "apps" : <NUMBER>,
    *    },
    *    "analysis" : [<NUMBER>,...]
    *    "apps" : [
    *      {
    *         "id" : <STRING>,
    *         "version" : <STRING>,
    *         "os" : <STRING>
    *      },...
    *    ],
    * }
    *
    *
    * @return
    */
  def toJson: JsValue = {
    JsObject(
      "_failed" -> JsObject(
        "analysis" -> JsNumber(failedAnalysis.length),
        "apps" -> JsNumber(failedApps.length)
      ),
      "analysis" -> JsArray(
        failedAnalysis.map(elem => JsNumber(elem.getId)).toVector),
      "apps" -> JsArray(
        failedApps.map { app =>
          JsObject(
            "id" -> JsString(app.id),
            "version" -> JsString(app.version),
            "os" -> JsString(app.os),
          )
        }.toVector
      )
    )

  }

}

object FailedAnalysisSummary {

  def apply(experiment: Experiment)(
      implicit database: Database): FailedAnalysisSummary = {
    new FailedAnalysisSummary(InterfaceAnalysis.get(experiment))
  }

}
