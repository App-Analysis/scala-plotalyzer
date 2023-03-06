package de.tubs.cs.ias.plotalyzer.analysis.tcfdetection

import spray.json.{
  DefaultJsonProtocol,
  JsObject,
  JsString,
  JsonParser,
  RootJsonFormat
}

import scala.io.Source
import scala.util.matching.Regex

case class TCFStaticDetectionConf(indicators: Map[String, String]) {
  def getIndicators: Map[String, Regex] =
    indicators.map(elem => elem._1 -> elem._2.r.unanchored)
}

case class TCFDynamicDetectionConf(vendors: JsObject) {

  def getVendorName(id: String): Option[String] = {
    vendors.fields.get(id) match {
      case Some(value) =>
        Some(value.asJsObject.fields("name").asInstanceOf[JsString].value)
      case None => None
    }
  }

}

case class TCFDetectionConf(static: TCFStaticDetectionConf,
                            dynamic: TCFDynamicDetectionConf)

object TCFDetectionConf extends DefaultJsonProtocol {

  implicit val tcfStaticDetectionConfFormat
    : RootJsonFormat[TCFStaticDetectionConf] = jsonFormat1(
    TCFStaticDetectionConf)

  implicit val tcfDynamicDetectionConfFormat
    : RootJsonFormat[TCFDynamicDetectionConf] = jsonFormat1(
    TCFDynamicDetectionConf)

  implicit val tcfDetectionConfFormat: RootJsonFormat[TCFDetectionConf] =
    jsonFormat2(TCFDetectionConf.apply)

  def apply(path: String): TCFDetectionConf = {
    val source = Source.fromFile(path)
    try {
      JsonParser(source.mkString).convertTo[TCFDetectionConf]
    } finally {
      source.close()
    }
  }

}
