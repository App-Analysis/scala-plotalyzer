package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object DeviceProvisioning extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://device-provisioning.googleapis.com/checkin".r
  )
  override val trackerName: String = "Firebase [Device Provisioning]"
  override val trackingCompany: String = "Firebase"

  override protected def prepare(request: Request): Option[JsObject] = {
    Some(JsonParser(request.content).asJsObject)
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("locale")(value => Language(value, PLAIN))
  }

  addMultiExtractor("checkin.iosbuild") { request =>
    request
      .getOptionalField[JsObject]("checkin") match {
      case Some(checkin) =>
        checkin.getOptionalField[JsObject]("iosbuild") match {
          case Some(iosbuild) =>
            List(
              Some(Model(iosbuild.getField[JsString]("model").value, PLAIN)),
              iosbuild.withOptionalFieldValue("os_version")(value =>
                OS(value.replace("_", " "), PLAIN))
            )
          case None => List()
        }
      case None => List()
    }
  }

  addExtractor("timezone") { request =>
    request.withOptionalFieldValue("time_zone", "timezone") { value =>
      TimeZone(value, PLAIN)
    }
  }
}
