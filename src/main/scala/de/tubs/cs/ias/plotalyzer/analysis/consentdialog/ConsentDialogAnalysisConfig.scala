package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import spray.json.{
  DefaultJsonProtocol,
  JsNumber,
  JsString,
  JsValue,
  JsonParser,
  RootJsonFormat
}
import scala.io.Source
import scala.util.matching.Regex

case class ButtonKeywords(clearAffirmative: List[String],
                          hiddenAffirmative: List[String],
                          clearNegative: List[String],
                          hiddenNegative: List[String])

case class Keywords(clearNegativeWords: List[String],
                    negators: List[String],
                    dialog: List[String],
                    link: List[String],
                    regularKeywords: List[String],
                    halfKeywords: List[String],
                    buttons: ButtonKeywords) {

  def convertToRegexp(strings: List[String],
                      enforceBoundaries: Boolean): List[Regex] = {
    strings
      .map { str =>
        if (enforceBoundaries) s"\\b${str.toLowerCase}\\b"
        else str.toLowerCase
      }
      .flatMap { str =>
        if (str.contains("_")) {
          List(str.r, str.replace("_", "").r)
        } else {
          List(str.r)
        }
      }
  }

  def getNegatorKeywords: List[Regex] =
    convertToRegexp(negators, enforceBoundaries = true)
  def getDialogKeywords: List[Regex] =
    convertToRegexp(dialog, enforceBoundaries = false)
  def getLinkKeywords: List[Regex] =
    convertToRegexp(link, enforceBoundaries = true)
  def getRegularKeywords: List[Regex] =
    convertToRegexp(regularKeywords, enforceBoundaries = false)
  def getHalfKeywords: List[Regex] =
    convertToRegexp(halfKeywords, enforceBoundaries = false)
  def getButtonClearAffirmativeKeywords: List[Regex] =
    convertToRegexp(buttons.clearAffirmative, enforceBoundaries = true)
  def getButtonHiddenAffirmativeKeywords: List[Regex] =
    convertToRegexp(buttons.hiddenAffirmative, enforceBoundaries = true)
  def getButtonClearNegativeKeywords: List[Regex] =
    convertToRegexp(clearNegativeWords ++ buttons.clearNegative,
                    enforceBoundaries = true)
  def getButtonHiddenNegativeKeywords: List[Regex] =
    convertToRegexp(buttons.hiddenNegative, enforceBoundaries = true)
  def getButtonNegatorKeywords: List[Regex] =
    convertToRegexp(negators ++ clearNegativeWords, enforceBoundaries = true)

}

case class ConsentDialogAnalysisDescriptors(initialDialog: String,
                                            initialTraffic: String,
                                            accept: String,
                                            reject: String)

case class ConsentDialogAnalysisConfig(
    keywordsConf: Keywords,
    descriptors: ConsentDialogAnalysisDescriptors,
    keywordScore: Int,
    lengthFactor: Double,
    sizeFactor: Double,
    colorDeltaEThreshold: Double)

object ConsentDialogAnalysisConfig {

  private object JSReader extends DefaultJsonProtocol {

    implicit val consentDialogAnalysisDescriptors
      : RootJsonFormat[ConsentDialogAnalysisDescriptors] =
      jsonFormat4(ConsentDialogAnalysisDescriptors)

    implicit object ConsentDialogAnalysisConfigFormat
        extends RootJsonFormat[ConsentDialogAnalysisConfig] {
      override def read(json: JsValue): ConsentDialogAnalysisConfig = {
        val fields = json.asJsObject.fields
        val keywords = {
          val source =
            Source.fromFile(
              fields("indicatorsFile").asInstanceOf[JsString].value)
          try {
            JsonParser(source.getLines().mkString("\n")).convertTo[Keywords]
          } finally {
            source.close()
          }
        }
        ConsentDialogAnalysisConfig(
          keywords,
          fields("descriptors").convertTo[ConsentDialogAnalysisDescriptors],
          fields("keywordScore").asInstanceOf[JsNumber].value.toInt,
          fields("lengthFactor").asInstanceOf[JsNumber].value.toDouble,
          fields("sizeFactor").asInstanceOf[JsNumber].value.toDouble,
          fields("colorDeltaEThreshold").asInstanceOf[JsNumber].value.toDouble
        )
      }

      override def write(obj: ConsentDialogAnalysisConfig): JsValue =
        throw new NotImplementedError()
    }

    implicit val buttonKeywordsFormat: RootJsonFormat[ButtonKeywords] =
      jsonFormat4(ButtonKeywords)

    implicit val keywordsFormat: RootJsonFormat[Keywords] = jsonFormat7(
      Keywords.apply)

  }

  import de.tubs.cs.ias.plotalyzer.analysis.consentdialog.ConsentDialogAnalysisConfig.JSReader._

  def apply(path: String): ConsentDialogAnalysisConfig = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.getLines().mkString("\n"))
        .convertTo[ConsentDialogAnalysisConfig]
    } finally {
      source.close()
    }
  }

}
