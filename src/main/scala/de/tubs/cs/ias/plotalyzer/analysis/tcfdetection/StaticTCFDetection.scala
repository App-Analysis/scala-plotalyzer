package de.tubs.cs.ias.plotalyzer.analysis.tcfdetection

import spray.json.{JsArray, JsObject, JsString, JsValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}
import scala.io.Source

class StaticTCFDetection(file: String,
                         conf: TCFDetectionConf,
                         os: String,
                         only: Set[String] = Set()) {

  private val iosPattern = "Payload/.+app/Frameworks/.+framework/".r.unanchored

  val result: Map[String, List[String]] = {
    val source = Source.fromFile(file)
    val future = Future.sequence {
      source.getLines().map { line =>
        val appName :: files = line.split(",").toList
        Future {
          if (only.isEmpty || only.contains(appName)) {
            appName -> {
              val considerFiles = os.toLowerCase match {
                case "ios" =>
                  files.filter(file => iosPattern.matches(file))
                case "android" => files
              }
              conf.static.getIndicators
                .filter {
                  case (_, indicator) =>
                    considerFiles.map(_.toLowerCase).exists(indicator.matches)
                }
                .keys
                .toList
            }
          } else {
            appName -> List("DONOTANALYZE")
          }
        }
      }
    }
    Await.result(future, Inf).toMap.filterNot(_._2.contains("DONOTANALYZE"))
  }

  def toJson: JsValue = {
    JsObject(result.map(elem =>
      elem._1 -> JsArray(elem._2.map(JsString.apply).toVector)))
  }

}
