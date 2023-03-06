package de.tubs.cs.ias.plotalyzer.utility

import spray.json.{JsObject, JsString, JsValue}

import scala.util.matching.Regex

case class StackTrace(trace: String) {

  val lines: List[String] = trace.split("\\n").toList

  def toJson: JsValue = {
    JsObject(
      lines.indices.map { index =>
        index.toString -> JsString(lines(index))
      }.toMap
    )
  }

  def getFirst(pattern: Regex): Option[String] = {
    lines.find(elem => pattern.unanchored.matches(elem))
  }

}
