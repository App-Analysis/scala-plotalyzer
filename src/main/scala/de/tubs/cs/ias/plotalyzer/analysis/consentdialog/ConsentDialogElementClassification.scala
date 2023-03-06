package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import de.tubs.cs.ias.plotalyzer.database.entities.InterfaceElement
import spray.json.{JsArray, JsObject, JsString}

import scala.util.matching.Regex

case class ConsentDialogElementClassification(
    dialogText: List[InterfaceElement],
    linkText: List[InterfaceElement],
    keywords: List[InterfaceElement],
    halfKeywords: List[InterfaceElement],
    clearAccept: List[InterfaceElement],
    unclearAccept: List[InterfaceElement],
    clearReject: List[InterfaceElement],
    unclearReject: List[InterfaceElement]) {

  override def toString: String =
    JsObject(
      "dialogText" -> JsArray(
        dialogText.map(elem => JsString(elem.getText)).toVector),
      "linkText" -> JsArray(
        linkText.map(elem => JsString(elem.getText)).toVector),
      "keywords" -> JsArray(
        keywords.map(elem => JsString(elem.getText)).toVector),
      "halfKeywords" -> JsArray(
        halfKeywords.map(elem => JsString(elem.getText)).toVector),
      "clearAccept" -> JsArray(
        clearAccept.map(elem => JsString(elem.getText)).toVector),
      "unclearAccept" -> JsArray(
        unclearAccept.map(elem => JsString(elem.getText)).toVector),
      "clearReject" -> JsArray(
        clearReject.map(elem => JsString(elem.getText)).toVector),
      "unclearReject" -> JsArray(
        unclearReject.map(elem => JsString(elem.getText)).toVector),
    ).prettyPrint

  def hasButtons: Boolean = hasAccept || hasReject

  def hasAccept: Boolean = clearAccept.nonEmpty || unclearAccept.nonEmpty
  def getAccept: List[InterfaceElement] = clearAccept ++ unclearAccept

  def getClearAccept: List[InterfaceElement] = clearAccept

  def hasReject: Boolean = clearReject.nonEmpty || unclearReject.nonEmpty
  def getReject: List[InterfaceElement] = clearReject ++ unclearReject

  def getClearReject: List[InterfaceElement] = clearReject

}

object ConsentDialogElementClassification {

  def apply(elements: List[InterfaceElement],
            keywords: Keywords,
            lengthFactor: Double): ConsentDialogElementClassification = {
    val clearAffirmative = filterElements(
      elements,
      keywords.getButtonClearAffirmativeKeywords,
      Some(lengthFactor),
      keywords.getNegatorKeywords)
    val hiddenAffirmative =
      filterElements(elements,
                     keywords.getButtonHiddenAffirmativeKeywords,
                     Some(lengthFactor),
                     keywords.getNegatorKeywords ++ List(
                       "continue (with|as)".r)) // if we have continue (with|as) it is a notice
    val clearNegative = filterElements(elements,
                                       keywords.getButtonClearNegativeKeywords,
                                       Some(lengthFactor),
                                       Nil)
    val hiddenNegative = filterElements(
      elements,
      keywords.getButtonHiddenNegativeKeywords,
      Some(lengthFactor),
      Nil)
    ConsentDialogElementClassification(
      filterElements(elements, keywords.getDialogKeywords, None, Nil),
      filterElements(elements, keywords.getLinkKeywords, None, Nil),
      filterElements(elements, keywords.getRegularKeywords, None, Nil),
      filterElements(elements, keywords.getHalfKeywords, None, Nil),
      clearAffirmative,
      (hiddenAffirmative.toSet -- clearAffirmative.toSet).toList, //if something has a clear affirmative keyword we are happy
      clearNegative,
      (hiddenNegative.toSet -- clearNegative.toSet).toList // if something has a clear negative keyword we are happy
    )
  }

  def filterElements(elements: List[InterfaceElement],
                     regex: List[Regex],
                     lengthFactor: Option[Double],
                     blockedRegexp: List[Regex]): List[InterfaceElement] = {
    // remove elements that are blocked
    elements
      .filterNot(element =>
        blockedRegexp.exists(
          _.findFirstIn(element.getText.toLowerCase.trim).nonEmpty))
      .flatMap { element =>
        regex
          .filter { regex =>
            // the matched text must not be longer than <lengthFactor> times the regexp (i.e., no infinitely large buttons)
            (if (lengthFactor.nonEmpty) {
               regex.regex.length.toDouble * lengthFactor.get > element.getText.length.toDouble
             } else true) &&
            // must match the regexp
            regex.findFirstIn(element.getText.toLowerCase.trim).nonEmpty
          }
          .map(regex => (regex, element))
      }
      .groupBy(_._2.getText) // multiple keywords can match the same button i.e., accept and continue both accept and continue
      .map(_._2.head._2)
      .toList
  }

}
