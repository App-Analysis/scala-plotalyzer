package de.tubs.cs.ias.plotalyzer.analysis.tcfdetection

import de.tubs.cs.ias.plotalyzer.database.entities.InterfaceAnalysis
import spray.json.{JsObject, JsString, JsValue, JsonParser}

class AppDynamicTCFDetection(analysis: List[InterfaceAnalysis]) {

  private val analysisTypes: Map[String, Map[String, JsValue]] = analysis.map {
    analysis =>
      analysis.getDescription -> extractIABTCFValues(analysis)
  }.toMap

  def extractIABTCFValues(analysis: InterfaceAnalysis): Map[String, JsValue] = {
    // we are assuming we only want the newest preferences
    try {
      if (analysis.getAppPreferences.nonEmpty) {
        val pref = analysis.getAppPreferences.maxBy(_.id).prefs
        val json = JsonParser(pref).asJsObject
        JsonParser(json.fields("preferences").asInstanceOf[JsString].value) match {
          case JsObject(fields) =>
            fields
              .filter(_._1.startsWith("IABTCF_"))
              .map(elem => elem._1 -> elem._2)
          case _ => Map()
        }
      } else {
        Map()
      }
    } catch {
      case _: Throwable =>
        Map()
    }
  }

  /** generate a JSON that can be pretty printed as specified below
    *
    * {
    *    <ANALYSIS> : {
    *      <IABKEY> : <IABVALUE>,...
    *    }, ...
    * }
    *
    * @return
    */
  def toJson: JsValue = {
    JsObject(
      analysisTypes.map { ana =>
        ana._1 -> JsObject(ana._2)
      }
    )
  }

}
