package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis
}
import spray.json.{JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

case class ConsentDialogAnalysis(analysis: List[InterfaceAnalysis],
                                 conf: ConsentDialogAnalysisConfig)
    extends LogSupport {

  private val appAnalysis: List[AppConsentDialogAnalysis] = {
    info(s"starting consent dialog analysis for ${analysis.length} analysis")
    val future = Future.sequence {
      val grouped = analysis.groupBy(_.getApp).toList
      info(s"we have analysis sets for ${grouped.length} different apps")
      grouped.map(set => Future(new AppConsentDialogAnalysis(set._2, conf)))
    }
    Await.result(future, Inf)
  }

  def getFinishedAnalysis: List[AppConsentDialogAnalysis] = appAnalysis

  def getFailedAnalysisIds: List[Int] = {
    appAnalysis.filter(_.getIssues.nonEmpty).flatMap(_.getAnalysisIds)
  }

  private def getErrorSummary: Map[String, Int] = {
    appAnalysis
      .flatMap(_.getIssues)
      .groupBy(elem => elem)
      .map(elem => elem._1 -> elem._2.length)
  }

  private def getHighLevelSummary: Map[String, Int] = {
    appAnalysis
      .filter(_.isValidConsentDialogMeasurement)
      .map(_.getConsentDialogType)
      .groupBy(elem => elem)
      .map(elem => elem._1 -> elem._2.length)
  }

  private def getHighLevelDialogSummary: JsValue = {
    val noErrorAnalysis =
      appAnalysis.filter(ana =>
        ana.isValidConsentDialogMeasurement && ana.getConsentDialogType == "Dialog")
    def count(analysis: List[AppConsentDialogAnalysis],
              func: AppConsentDialogAnalysis => JsValue): Int = {
      analysis.map { ana =>
        func(ana) match {
          case boolean: JsBoolean => if (boolean.value) 1 else 0
          case JsNull             => 0
          case _                  => throw new RuntimeException("expected JsNull or JsBoolean")
        }
      }.sum
    }
    JsObject(
      "accept" -> JsObject(
        "exists" -> JsNumber(noErrorAnalysis.count(_.hasAccept)),
        "clear" -> JsNumber(noErrorAnalysis.count(_.hasUniqueClearAccept)),
        "equivocal" -> JsNumber(
          count(noErrorAnalysis, _.getAcceptEquivocalJson)),
        "larger" -> JsNumber(count(noErrorAnalysis, _.getAcceptLargerJson)),
        "highlighted" -> JsNumber(
          count(noErrorAnalysis, _.getAcceptHighlightedJson))
      ),
      "reject" -> JsObject(
        "exists" -> JsNumber(noErrorAnalysis.count(_.hasReject)),
        "clear" -> JsNumber(noErrorAnalysis.count(_.hasUniqueClearReject)),
        "equivocal" -> JsNumber(
          count(noErrorAnalysis, _.getRejectEquivocalJson)),
        "closesAfter" -> JsNumber(
          count(noErrorAnalysis, _.getRejectStopsAppJson))
      )
    )
  }

  private def getSussApps: JsObject = {
    val suss = List(
      ConsentDialogAnalysisIssues.NONE_WITH_ACCEPT_REJECT_ANALYSIS)
    JsObject(
      appAnalysis
        .map { analysis =>
          analysis.getIssues.find(elem => suss.contains(elem)) match {
            case Some(value) => Some(analysis.getApp.id -> JsString(value))
            case None        => None
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
        .toMap)
  }

  /** generate the JsObect that pretty prints the result json
    *
    * {
    *    _count : <NUMBER>,
    *    _success : <NUMBER>,
    *    _sussapps : {
    *      <APPID> : <STRING>,...
    *     }
    *    errorSummary : {
    *      <ERROR> : <NUMBER>, ...
    *    }
    *    highLevelSummary : {
    *      <DIALOGTYPE> : <NUMBER>,...
    *    },
    *    highLevelDialogSummary : {
    *      <HIGHLEVELDIALOGSUMMARY>
    *    }
    *    perAppResults : {
    *      appId : <APPCOSNENTDIALOGANALYSIS>,....
    *    }
    * }
    *
    * @return the JsObject that pretty prints to the above specifications
    */
  def toJson: JsObject = {
    JsObject(
      "_count" -> JsNumber(appAnalysis.length),
      "_success" -> JsNumber(
        appAnalysis.count(_.isValidConsentDialogMeasurement)),
      "_sussapps" -> getSussApps,
      "errorSummary" -> JsObject(
        getErrorSummary.map(elem => elem._1 -> JsNumber(elem._2))),
      "highLevelSummary" -> JsObject(
        getHighLevelSummary.map(elem => elem._1 -> JsNumber(elem._2))),
      "highLevelDialogSummary" -> getHighLevelDialogSummary,
      "perAppresults" -> JsObject(
        appAnalysis.map(elem => elem.getAppIdString -> elem.toJson).toMap
      )
    )
  }

}

object ConsentDialogAnalysis {

  def apply(experiment: Experiment, config: String, only: Set[String] = Set())(
      implicit database: Database): ConsentDialogAnalysis = {
    val analysis = InterfaceAnalysis
      .get(experiment)
      .filter(ana => only.isEmpty || only.contains(ana.getApp.toString))
    new ConsentDialogAnalysis(analysis,
                              ConsentDialogAnalysisConfig.apply(config))
  }

}
