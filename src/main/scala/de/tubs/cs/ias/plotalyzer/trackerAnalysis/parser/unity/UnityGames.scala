package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.unity

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsNumber, JsObject, JsString, JsValue, JsonParser}
import wvlet.log.LogSupport

import scala.util.matching.Regex

object UnityGames extends EndpointParser with LogSupport {

  override val endpointURLs: List[Regex] = List(
    "https://publisher-config.unityads.unity3d.com/games/[0-9]+/configuration".r,
    "https://auction.unityads.unity3d.com/v4/test/games/[0-9]+/requests".r,
    "https://configv2.unityads.unity3d.com/games/[0-9]+/configuration.*".r,
    "https://auction-load.unityads.unity3d.com/v6/games/[0-9]+/requests".r
  )
  override val trackerName: String = "unity [games]"
  override val trackingCompany: String = "Unity3d"

  override protected def prepare(request: Request): Option[JsObject] = {
    val contentJson = Option(request.content) match {
      case Some(value) if value.nonEmpty => JsonParser(value).asJsObject
      case _                             => JsObject()
    }
    val urlJson = request.getPath.split('?').toList match {
      case Nil               => JsObject()
      case _ :: Nil          => JsObject()
      case _ :: query :: Nil => parseQueryString(query)
      case _ =>
        warn(s"weirdly formed request url ${request.getFullUrl}")
        JsObject()
    }
    val ret = DeepMerge.merge(contentJson, urlJson)
    if (ret.fields.nonEmpty)
      Some(ret)
    else
      None
  }

  addExtractor("model") { request =>
    val deviceMaker = request.getOptionalField[JsString]("deviceMake") match {
      case Some(value) => value.value
      case None        => ""
    }
    val deviceModel = request.getOptionalField[JsString]("deviceModel") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = deviceMaker + deviceModel
    if (str.nonEmpty)
      Some(Model(str, PLAIN))
    else
      None
  }

  addExtractor("ext?.ios_jailbroken") { request =>
    request.getOptionalField[JsObject]("ext") match {
      case Some(ext) =>
        ext.withOptionalFieldValue("ios_jailbroken")(value =>
          Rooted(value, PLAIN))
      case None => None
    }
  }

  addExtractor("ext?.device_battery_level") { request =>
    request.getOptionalField[JsObject]("ext") match {
      case Some(ext) =>
        ext.withOptionalFieldValue("device_battery_charging")(value =>
          BatteryPercentage(value, PLAIN))
      case None =>
        request.withOptionalFieldValue("batteryLevel")(value =>
          BatteryPercentage(value, PLAIN))
    }
  }

  addExtractor("webviewUA") {
    _.withOptionalFieldValue("webviewUa")(value => UserAgent(value, PLAIN))
  }

  addExtractor("deviceFreeSpace") {
    _.withOptionalFieldValue("deviceFreeSpace")(value => DiskFree(value, PLAIN))
  }

  addExtractor("networkOperatorName") {
    _.withOptionalFieldValue("networkOperatorName")(value =>
      Carrier(value, PLAIN))
  }

  addExtractor("volume") {
    _.withOptionalFieldValue("volume")(value => Volume(value, PLAIN))
  }

  addExtractor("totalSpace") {
    _.withOptionalFieldValue("totalSpace")(value => DiskTotal(value, PLAIN))
  }

  addExtractor("timeZone") {
    _.withOptionalFieldValue("timeZone")(value => TimeZone(value, PLAIN))
  }

  addExtractor("gameSessionId") { request =>
    request.getOptionalField[JsValue]("gameSessionId") match {
      case Some(value: JsString) =>
        UUID.createAppropriateIdentifier(value.value, PLAIN)
      case Some(value: JsNumber) =>
        Some(OtherIdentifier(value.value.toString(), PLAIN))
      case None =>
        None
    }
  }

  addExtractor("bundleId") {
    _.withOptionalFieldValue("bundleId")(value => AppID(value, PLAIN))
  }

  addExtractor("bundleVersion") {
    _.withOptionalFieldValue("bundleVersion")(value => AppVersion(value, PLAIN))
  }

  addExtractor("deviceModel") {
    _.withOptionalFieldValue("deviceModel")(value => Model(value, PLAIN))
  }

  addExtractor("idfi") {
    _.withOptionalFieldValue("idfi")(value => UUID(value, PLAIN))
  }

  addExtractor("advertisingTrackingId") { request =>
    request.withOptionalFieldValue("advertisingTrackingId")(value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("analyticsSessionId") {
    _.withOptionalFieldValue("analyticsSessionId")(value => UUID(value, PLAIN))
  }

  addExtractor("analyticsUserId") { request =>
    request.withOptionalFieldValue("analyticsUserId")(value =>
      UUID(value, PLAIN))
  }

  addExtractor("connectionType") { request =>
    request.withOptionalFieldValue("connectionType")(value =>
      NetworkConnectionType(value, PLAIN))
  }

  addExtractor("screenWidth") { request =>
    request.withOptionalFieldValue("screenWidth")(value => Width(value, PLAIN))
  }

  addExtractor("screenHeight") { request =>
    request.withOptionalFieldValue("screenHeight")(value =>
      Height(value, PLAIN))
  }

  addExtractor("rooted") { request =>
    request.withOptionalFieldValue("rooted")(value => Rooted(value, PLAIN))
  }

  addExtractor("platform") { request =>
    val platform = request
      .getOptionalField[JsString]("platform")
      .getOrElse(JsString(""))
      .value
    val osv = request
      .getOptionalField[JsString]("osVersion")
      .getOrElse(JsString(""))
      .value
    val apilevel = request
      .getOptionalField[JsString]("apiLevel")
      .getOrElse(JsString(""))
      .value
    val str = platform + osv + apilevel
    if (str.nonEmpty) {
      Some(OS(str, PLAIN))
    } else {
      None
    }
  }

  addExtractor("language") { request =>
    request.withOptionalFieldValue("language")(value => Language(value, PLAIN))
  }
}
