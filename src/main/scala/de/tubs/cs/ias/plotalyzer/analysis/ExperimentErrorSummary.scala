package de.tubs.cs.ias.plotalyzer.analysis

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysisError
}
import spray.json.{JsArray, JsNumber, JsObject, JsValue}

class ExperimentErrorSummary(experiment: Experiment)(
    implicit database: Database) {

  /**
    * {
    *    experimentErrorsCount : <NUMBER>,
    *    interfaceAnalysisErrorCount : <NUMBER>,
    *    experimentErrors : [
    *      <EXPERIMENT_ERROR>,...
    *    ]
    *    interfaceAnalysisErrors : {
    *       "interfaceAnalysisId" : {
    *          <InterfaceAnalysisError>,...
    *       },...
    *    }
    * }
    *
    * @return
    */
  def toJson: JsValue = {
    val experimentErrors = experiment.getExperimentErrors
    val interfaceAnalysisErrors =
      InterfaceAnalysisError.getInterfaceAnalysisErrors(
        experiment.getInterfaceAnalysisIds)
    JsObject(
      "_experimentErrorsCount" -> JsNumber(experimentErrors.length),
      "_analysisErrorCount" -> JsNumber(
        interfaceAnalysisErrors.map(_.getAnalysisId).toSet.size),
      "experimentErrors" -> JsArray(
        experimentErrors.map(_.toJson).toVector
      ),
      "interfaceAnalysisErrors" -> JsObject {
        interfaceAnalysisErrors.map { elem =>
          elem.getId.toString -> elem.toJson
        }.toMap
      }
    )
  }
}

object ExperimentErrorSummary {

  def apply(experiment: Experiment)(
      implicit database: Database): ExperimentErrorSummary =
    new ExperimentErrorSummary(experiment)

}
