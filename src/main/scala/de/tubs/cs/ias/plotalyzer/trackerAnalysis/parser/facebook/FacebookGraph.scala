package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.facebook

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.utility.json.DeepMerge
import spray.json.{JsObject, JsString, JsonParser}
import java.net.URLDecoder
import scala.util.matching.Regex

object FacebookGraph extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://(www|web).facebook.com/adnw_sync2".r,
    "https://graph.facebook.com/network_ads_common".r
  )
  override val trackerName: String = "facebook [graph]"
  override val trackingCompany: String = "Facebook"

  override protected def prepare(request: Request): Option[JsObject] = {
    if ("https://graph.facebook.com/network_ads_common".r.matches(
          request.getUrl)) {
      Option(request.content) match {
        case Some(_) =>
          val jsObject = parseQueryFromBody(request).get
          val VALPARAMS = jsObject.fields.get("VALPARAMS") match {
            case Some(value: JsString) =>
              JsonParser(URLDecoder.decode(value.value)).asJsObject
            case _ => JsObject()
          }
          val ANALOG = jsObject.fields.get("ANALOG") match {
            case Some(value: JsString) =>
              JsonParser(URLDecoder.decode(value.value)).asJsObject
            case _ => JsObject()
          }
          val ret = JsObject(
            DeepMerge
              .merge(jsObject, VALPARAMS, ANALOG)
              .fields
              .filterNot(elem => Set("ANALOG", "VALPARAMS").contains(elem._1)))
          if (ret.fields.nonEmpty)
            Some(ret)
          else
            None
        case None => None
      }
    } else {
      Option(request.content) match {
        case Some(_) =>
          parseQueryFromBody(request) match {
            case Some(obj) =>
              val payload = JsonParser(URLDecoder.decode(
                obj.fields("payload").asInstanceOf[JsString].value)).asJsObject
              payload.getOptionalField[JsObject]("request") match {
                case Some(request) =>
                  val VALPARAMS = request.fields.get("VALPARAMS") match {
                    case Some(value: JsString) =>
                      JsonParser(URLDecoder.decode(value.value)).asJsObject
                    case _ => JsObject()
                  }
                  val ret = JsObject(
                    DeepMerge
                      .merge(request, VALPARAMS)
                      .fields
                      .filterNot(_._1 == "VALPARAMS"))
                  Some(ret)
                case None => None
              }

            case None => None
          }
        case None => None
      }
    }
  }

  addExtractor("free_space") {
    _.withOptionalFieldValue("free_space")(value => DiskFree(value, PLAIN))
  }

  addExtractor("battery") {
    _.withOptionalFieldValue("battery")(value =>
      BatteryPercentage(value, PLAIN))
  }

  addExtractor("availableMemory") {
    _.withOptionalFieldValue("availableMemory")(value => RamFree(value, PLAIN))
  }

  addExtractor("totalMemory") {
    _.withOptionalFieldValue("availableMemory")(value => RamTotal(value, PLAIN))
  }

  addExtractor("accelerometer_x") {
    _.withOptionalFieldValue("accelerometer_x")(value =>
      AccelerometerX(value, PLAIN))
  }

  addExtractor("accelerometer_y") {
    _.withOptionalFieldValue("accelerometer_y")(value =>
      AccelerometerY(value, PLAIN))
  }

  addExtractor("accelerometer_z") {
    _.withOptionalFieldValue("accelerometer_z")(value =>
      AccelerometerZ(value, PLAIN))
  }

  addExtractor("IDFA") {
    _.withOptionalFieldValue("IDFA")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("SDK_VERSION") {
    _.withOptionalFieldValue("SDK_VERSION")(value => SDKVersion(value, PLAIN))
  }

  addExtractor("SESSION_ID") {
    _.withOptionalFieldValue("SESSION_ID") { value =>
      UUID(value, PLAIN)
    }
  }

  addExtractor("SCREEN_WIDTH") {
    _.withOptionalFieldValue("SCREEN_WIDTH")(value => Width(value, PLAIN))
  }

  addExtractor("CARRIER") {
    _.withOptionalFieldValue("CARRIER")(value => Carrier(value, PLAIN))
  }

  addExtractor("MODEL") {
    _.withOptionalFieldValue("MODEL")(value => Model(value, PLAIN))
  }

  addExtractor("LOCALE") {
    _.withOptionalFieldValue("LOCALE")(value => Language(value, PLAIN))
  }

  addExtractor("ROOTED") {
    _.withOptionalFieldValue("ROOTED")(value => Rooted(value, PLAIN))
  }

  addExtractor("is_emu") {
    _.withOptionalFieldValue("is_emu")(value => Emulator(value, PLAIN))
  }

  addExtractor("OS") {
    _.withOptionalFieldValue("OS")(value => OS(value, PLAIN))
  }

  addExtractor("CLIENT_REQUEST_ID") {
    _.withOptionalFieldValue("CLIENT_REQUEST_ID") { value =>
      UUID(value, PLAIN)
    }
  }

  addExtractor("BUNDLE") {
    _.withOptionalFieldValue("BUNDLE")(value => AppID(value, PLAIN))
  }

}
