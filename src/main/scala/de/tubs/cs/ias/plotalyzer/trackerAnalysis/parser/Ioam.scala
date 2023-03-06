package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser

import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.trackerAnalysis._
import spray.json.{JsObject, JsString, JsonParser}

import scala.util.matching.Regex

object Ioam extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://config.ioam.de/appcfg.php".r
  )
  override val trackerName: String = "ioam"
  override val trackingCompany: String = "Ioam"

  override protected def prepare(request: Request): Option[JsObject] = {
    Some(JsonParser(request.content).asJsObject)
  }

  addExtractor("application?.package") { request =>
    request.getOptionalField[JsObject]("application") match {
      case Some(application) =>
        application.withOptionalFieldValue("package", "bundleIdentifier")(
          value => AppID(value, PLAIN))
      case None => None
    }
  }

  addExtractor("applicatino?.package") { request =>
    request.getOptionalField[JsObject]("application") match {
      case Some(appliaction) =>
        appliaction.withOptionalFieldValue("versionName", "bundleVersion")(
          value => AppVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("library?.libVersion") { request =>
    request.getOptionalField[JsObject]("library") match {
      case Some(library) =>
        library.withOptionalFieldValue("libVersion")(value =>
          SDKVersion(value, PLAIN))
      case None => None
    }
  }

  addExtractor("client?.advertisingIdentifier") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.getOptionalField[JsObject]("uuids") match {
          case Some(uuids) =>
            uuids.withOptionalFieldValue("advertisingIdentifier")(value =>
              GlobalOSAdvertisingIdentifier(value, PLAIN))
          case None => None
        }
      case None => None
    }
  }

  addExtractor("client?.uuids?.installationId") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.getOptionalField[JsObject]("uuids") match {
          case Some(uuids) =>
            uuids.getOptionalField[JsString]("installationId") match {
              case Some(value) =>
                UUID.createAppropriateIdentifier(value.value, PLAIN)
              case None =>
                None
            }
          case None => None
        }
      case None => None
    }
  }

  addExtractor("client?.uuids?.vendorIdentifier") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.getOptionalField[JsObject]("uuids") match {
          case Some(uuids) =>
            uuids.withOptionalFieldValue("vendorIdentifier")(value =>
              LocalOSAdvertisingIdentifier(value, PLAIN))
          case None => None
        }
      case None => None
    }
  }

  addExtractor("client?.uuids?.androidId") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.getOptionalField[JsObject]("uuids") match {
          case Some(uuids) =>
            uuids.withOptionalFieldValue("androidId")(value =>
              UUID(value, PLAIN))
          case None => None
        }
      case None => None
    }
  }

  addExtractor("client?.platform") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.withOptionalFieldValue("platform")(value => Model(value, PLAIN))
      case None => None
    }
  }

  addExtractor("client?.os") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        val os =
          client
            .getOptionalField[JsString]("osIdentifier")
            .getOrElse(JsString(""))
            .value
        val osv =
          client
            .getOptionalField[JsString]("osVersion")
            .getOrElse(JsString(""))
            .value
        val str = os + osv
        if (str.nonEmpty)
          Some(OS(str, PLAIN))
        else
          None
      case None => None
    }
  }

  addExtractor("client?.language") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.withOptionalFieldValue("language")(value =>
          Language(value, PLAIN))
      case None => None
    }
  }

  addExtractor("client?.carrier") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.withOptionalFieldValue("carrier")(value => Carrier(value, PLAIN))
      case None => None
    }
  }

  addExtractor("client?.country") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.withOptionalFieldValue("country")(value => Country(value, PLAIN))
      case None => None
    }
  }

  addMultiExtractor("client?.screen?.resolution") { request =>
    request.getOptionalField[JsObject]("client") match {
      case Some(client) =>
        client.getOptionalField[JsObject]("screen") match {
          case Some(screen) =>
            screen.getOptionalField[JsString]("resolution") match {
              case Some(resolution) =>
                val width :: height :: Nil = resolution.value.split("x").toList
                List(
                  Some(Width(width, PLAIN)),
                  Some(Height(height, PLAIN))
                )
              case None => List()
            }
          case None => List()
        }
      case None => List()
    }
  }
}
