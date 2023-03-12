package de.tubs.cs.ias.plotalyzer.plugins

import spray.json.JsValue

trait AnalysisPlugin {
  def analyze(context: AnalysisContext): Either[Exception, JsValue]

}
