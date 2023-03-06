package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import spray.json.JsObject

case class MissingJsObjectField(field: String, obj: JsObject)
    extends Throwable {
  override def getMessage: String =
    s"the filed $field is missing from ${obj.prettyPrint}"
}
