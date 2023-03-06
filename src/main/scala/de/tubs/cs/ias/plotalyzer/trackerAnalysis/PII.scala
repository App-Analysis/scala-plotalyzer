package de.tubs.cs.ias.plotalyzer.trackerAnalysis

import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.{Encoding, PLAIN}
import spray.json.{JsObject, JsString, JsValue}

object Encoding extends Enumeration {
  type Encoding = Value
  val PLAIN, HASHED, BASE64 = Value
}

trait PII {
  val value: String
  val encoding: Encoding

  def getPIIName: String = {
    this.getClass.getName.split('.').last
  }

  def toJson: JsValue = {
    JsObject(
      "type" -> JsString(getPIIName),
      "value" -> JsString(this.value)
    )
  }
}

case class RequestParsingIssue(override val value: String) extends PII {
  override val encoding: Encoding = PLAIN
}
case class UnexpectedValue(override val value: String,
                           override val encoding: Encoding)
    extends PII

case class MissingExpectedValue(field: String, obj: JsObject) extends PII {
  override val value: String = s"missing $field in ${obj.prettyPrint}"
  override val encoding: Encoding = PLAIN
}
case class ExtractorFailure(endpoint: String,
                            name: String,
                            obj: JsObject,
                            msg: String)
    extends PII {
  override val value: String =
    s"Extractor $name of $endpoint failed with $msg for ${obj.prettyPrint}"
  override val encoding: Encoding = PLAIN
}
case class GlobalOSAdvertisingIdentifier(override val value: String,
                                         override val encoding: Encoding)
    extends PII
case class LocalOSAdvertisingIdentifier(override val value: String,
                                        override val encoding: Encoding)
    extends PII
case class UUID(override val value: String, override val encoding: Encoding)
    extends PII {

  assert(UUID.isUUID(value))

}

object UUID {

  def isUUID(str: String): Boolean =
    "[a-z0-9]{8}-?[a-z0-9]{4}-?[a-z0-9]{4}-?[a-z0-9]{4}-?[a-z0-9]{12}".r
      .matches(str.toLowerCase)

  def createAppropriateIdentifier(str: String,
                                  encoding: Encoding): Option[PII] = {
    if (isUUID(str)) {
      Some(UUID(str, encoding))
    } else if (str.nonEmpty) {
      Some(OtherIdentifier(str, encoding))
    } else {
      None
    }
  }

}

case class OtherIdentifier(override val value: String,
                           override val encoding: Encoding)
    extends PII

case class Model(override val value: String, override val encoding: Encoding)
    extends PII
case class OS(override val value: String, override val encoding: Encoding)
    extends PII
case class DeviceName(override val value: String,
                      override val encoding: Encoding)
    extends PII

case class Language(override val value: String, override val encoding: Encoding)
    extends PII
case class TimeZone(override val value: String, override val encoding: Encoding)
    extends PII
case class UserAgent(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class Orientation(override val value: String,
                       override val encoding: Encoding)
    extends PII
case class Carrier(override val value: String, override val encoding: Encoding)
    extends PII
case class Rooted(override val value: String, override val encoding: Encoding)
    extends PII
case class Emulator(override val value: String, override val encoding: Encoding)
    extends PII
case class Width(override val value: String, override val encoding: Encoding)
    extends PII
case class Height(override val value: String, override val encoding: Encoding)
    extends PII
case class Roaming(override val value: String, override val encoding: Encoding)
    extends PII
case class Uptime(override val value: String, override val encoding: Encoding)
    extends PII
case class RamTotal(override val value: String, override val encoding: Encoding)
    extends PII
case class RamFree(override val value: String, override val encoding: Encoding)
    extends PII
case class NetworkConnectionType(override val value: String,
                                 override val encoding: Encoding)
    extends PII
case class SignalStrengthCellular(override val value: String,
                                  override val encoding: Encoding)
    extends PII
case class SignalStrengthWifi(override val value: String,
                              override val encoding: Encoding)
    extends PII
case class IsCharging(override val value: String,
                      override val encoding: Encoding)
    extends PII
case class BatteryPercentage(override val value: String,
                             override val encoding: Encoding)
    extends PII
case class BatteryState(override val value: String,
                        override val encoding: Encoding)
    extends PII
case class DiskTotal(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class DiskFree(override val value: String, override val encoding: Encoding)
    extends PII
case class AccelerometerX(override val value: String,
                          override val encoding: Encoding)
    extends PII
case class AccelerometerY(override val value: String,
                          override val encoding: Encoding)
    extends PII
case class AccelerometerZ(override val value: String,
                          override val encoding: Encoding)
    extends PII
case class RotationX(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class RotationY(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class RotationZ(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class MacAddress(override val value: String,
                      override val encoding: Encoding)
    extends PII
case class Architecture(override val value: String,
                        override val encoding: Encoding)
    extends PII
case class DarkMode(override val value: String, override val encoding: Encoding)
    extends PII
case class LocalIp(override val value: String, override val encoding: Encoding)
    extends PII
case class Volume(override val value: String, override val encoding: Encoding)
    extends PII
case class Country(override val value: String, override val encoding: Encoding)
    extends PII
case class Latitude(override val value: String, override val encoding: Encoding)
    extends PII
case class Longitude(override val value: String,
                     override val encoding: Encoding)
    extends PII
case class PublicIP(override val value: String, override val encoding: Encoding)
    extends PII
case class SDKVersion(override val value: String,
                      override val encoding: Encoding)
    extends PII
case class AppID(override val value: String, override val encoding: Encoding)
    extends PII
case class AppVersion(override val value: String,
                      override val encoding: Encoding)
    extends PII
case class InForeground(override val value: String,
                        override val encoding: Encoding)
    extends PII
case class CurrentlyViewed(override val value: String,
                           override val encoding: Encoding)
    extends PII
case class EmptyRequest(url: String) extends PII {
  override val value: String = url
  override val encoding: Encoding = PLAIN
}
