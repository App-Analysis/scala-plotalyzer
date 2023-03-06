package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.facebook

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{
  AppID,
  EndpointParser,
  GlobalOSAdvertisingIdentifier,
  UUID
}
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsArray, JsObject, JsString, JsonParser}

import java.net.URLDecoder
import scala.util.matching.Regex

object FacebookGraphV extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://graph.facebook.com/v\\d{1,2}.0/[0-9]+".r,
    "https://graph.facebook.com/v.\\d{1,2}.0/[0-9]+/activities".r
  )
  override val trackerName: String = "Facebook [GraphV]"
  override val trackingCompany: String = "Facebook"

  override protected def prepare(request: Request): Option[JsObject] = {
    val query: JsObject = parseQueryFromPath(request).getOrElse(JsObject())
    val content = if (request.getPath.endsWith("/activities")) {
      parseQueryFromBody(request) match {
        case Some(content) =>
          content.fields.get("format") match {
            case Some(value: JsString) if value.value == "json" =>
              val custom = content.fields.get("custom_events") match {
                case Some(value: JsString) =>
                  JsonParser(URLDecoder.decode(value.value)) match {
                    case JsObject(fields)  => JsObject(fields)
                    case JsArray(elements) => elements.head.asJsObject
                    case _ =>
                      throw new RuntimeException("expected array or object")
                  }
                case _ => JsObject()
              }
              JsObject(
                DeepMerge
                  .merge(content, custom)
                  .fields
                  .filterNot(_._1 == "custom_events"))
            case _ =>
              JsObject()
          }
        case None =>
          JsObject()
      }
    } else {
      JsObject()
    }
    val ret = DeepMerge.merge(query, content)
    Some(ret)
  }

  addExtractor("anon_id") {
    _.withOptionalFieldValue("anon_id") { value =>
      if (value.startsWith("XZ"))
        UUID(value.substring("XZ".length), PLAIN)
      else
        UUID(value, PLAIN)
    }
  }

  addExtractor("advertiser_id") {
    _.withOptionalFieldValue("advertiser_id")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("application_package_name") {
    _.withOptionalFieldValue("application_package_name")(value =>
      AppID(value, PLAIN))
  }

  addMultiExtractor("extinfo.ARR") { _ =>
    List() //this does contain interesting stuff but the array is irregular
  }
}
