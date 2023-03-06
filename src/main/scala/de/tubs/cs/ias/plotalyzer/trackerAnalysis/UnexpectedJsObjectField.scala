package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import spray.json.{JsObject, JsValue}

case class UnexpectedJsObjectField(field: JsValue,
                                   obj: JsObject,
                                   expected: String =
                                     "expected value carrying field")
    extends Throwable {

  override def getMessage: String =
    s"$expected but got ${field.prettyPrint} in ${obj.prettyPrint}"

}
