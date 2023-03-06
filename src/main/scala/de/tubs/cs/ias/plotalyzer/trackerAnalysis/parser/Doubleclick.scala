package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Doubleclick extends EndpointParser {

  override val endpointURLs: List[Regex] = List(
    "https://googleads.g.doubleclick.net/mads/gma".r,
    "https://googleads.g.doubleclick.net/getconfig/pubsetting".r
  )
  override val trackerName: String = "doubleclick"
  override val trackingCompany: String = "Doubleclick"

  override protected def prepare(request: Request): Option[JsObject] = {
    if (request.method == "POST") {
      Some(JsonParser(request.content).asJsObject)
    } else {
      parseQueryFromPath(request)
    }
  }

  addExtractor("model") { request =>
    val plat = request
      .getOptionalField[JsString]("platform")
      .getOrElse(JsString(""))
      .value
    val sub = request
      .getOptionalField[JsString]("submodel")
      .getOrElse(JsString(""))
      .value
    val str = plat + sub
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("os") { request =>
    val os = request
      .getOptionalField[JsString]("sys_name")
      .getOrElse(JsString(""))
      .value
    val osv = request
      .getOptionalField[JsString]("os_version")
      .getOrElse(JsString(""))
      .value
    val str = os + osv
    if (str.nonEmpty)
      Some(OS(os + osv, PLAIN))
    else
      None
  }

  addExtractor("volume") { request =>
    request.withOptionalFieldValue("android_app_volume", "ios_app_volume")(
      value => Volume(value, PLAIN))
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("hl")(value => Language(value, PLAIN))
  }

  addExtractor("net") { request =>
    request.withOptionalFieldValue("net")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("binary_arch") { request =>
    request.withOptionalFieldValue("binary_arch")(value =>
      Architecture(value, PLAIN))
  }

  addExtractor("rooted") { request =>
    request.withOptionalFieldValue("ios_jb")(value => Rooted(value, PLAIN))
  }

  addExtractor("appame") { request =>
    request.withOptionalFieldValue("app_name", "_package_name", "an", "msid")(
      value => AppID(value, PLAIN))
  }

  addExtractor("dtsdk") { request =>
    request.withOptionalFieldValue("dtsdk")(value => SDKVersion(value, PLAIN))
  }
}
