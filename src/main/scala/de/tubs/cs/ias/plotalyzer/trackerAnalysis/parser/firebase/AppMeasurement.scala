package de.tubs.cs.ias.plotalyzer.trackerAnalysis.parser.firebase

import com.google.protobuf.{ByteString, InvalidProtocolBufferException}
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.Encoding.PLAIN
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.{
  AppID,
  AppVersion,
  EndpointParser,
  GlobalOSAdvertisingIdentifier,
  LocalOSAdvertisingIdentifier,
  OS
}
import de.tubs.cs.ias.plotalyzer.trackerAnalysis.JsonObjectExtension.ExtendedJsObject
import de.tubs.cs.ias.plotalyzer.utility.DeepMerge
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue}

import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}
import scala.util.matching.Regex

object AppMeasurement extends EndpointParser {
  override val endpointURLs: List[Regex] = List(
    "https://app-measurement.com/a".r
  )
  override val trackerName: String = "firebase [AppMeasurement]"
  override val trackingCompany: String = "Firebase"

  def createJsonObject(
      fieldSet: com.google.protobuf.UnknownFieldSet): JsObject = {
    val retMap: MMap[Int, ListBuffer[JsValue]] = MMap()
    def addOrCreate(number: Int, element: JsValue): Unit = {
      retMap.get(number) match {
        case Some(value) => value.addOne(element)
        case None        => retMap.addOne(number -> ListBuffer(element))
      }
    }
    def addVarInt(number: Int, list: List[java.lang.Long]): Unit = {
      list.foreach(elem => addOrCreate(number, JsNumber(elem)))
    }
    def addFixed32(number: Int, list: List[java.lang.Integer]): Unit = {
      list.foreach(elem =>
        addOrCreate(number, JsString(s"0x%08x".format(elem))))
    }
    def addFixed64(number: Int, list: List[java.lang.Long]): Unit = {
      list.foreach(elem =>
        addOrCreate(number, JsString(s"0x%016x".format(elem))))
    }
    def addVariableLength(number: Int, list: List[ByteString]): Unit = {
      list.foreach { elem =>
        try {
          val message = com.google.protobuf.UnknownFieldSet.parseFrom(elem)
          addOrCreate(number, createJsonObject(message))
        } catch {
          case _: InvalidProtocolBufferException =>
            addOrCreate(
              number,
              JsString(com.google.protobuf.TextFormat.escapeBytes(elem)))
        }
      }
    }
    def addUnknownFieldSet(
        number: Int,
        fieldSet: com.google.protobuf.UnknownFieldSet): Unit = {
      addOrCreate(number, createJsonObject(fieldSet))
    }
    fieldSet.asMap().asScala.foreach {
      case (number: Integer,
            value: com.google.protobuf.UnknownFieldSet.Field) =>
        addVarInt(number, value.getVarintList.asScala.toList)
        addFixed32(number, value.getFixed32List.asScala.toList)
        addFixed64(number, value.getFixed64List.asScala.toList)
        addVariableLength(number, value.getLengthDelimitedList.asScala.toList)
        value.getGroupList.forEach(gle => addUnknownFieldSet(number, gle))
    }
    JsObject(
      retMap.map(elem => elem._1.toString -> JsArray(elem._2.toVector)).toMap
    )
  }

  override protected def prepare(request: Request): Option[JsObject] = {
    try {
      Option(request.contentRaw) match {
        case Some(value) =>
          val protoBufJson = createJsonObject(
            com.google.protobuf.UnknownFieldSet.parseFrom(value))
          val messages = protoBufJson.fields.get("1") match {
            case Some(messages: JsArray) =>
              messages.elements.map(_.asJsObject).toList
            case Some(messages: JsObject) => List(messages)
            case _                        => List()
          }
          val ret = DeepMerge.arrayReduce(DeepMerge.merge(messages: _*))
          Some(ret)
        case None => None
      }
    } catch {
      case _: Throwable => None
    }
  }

  addExtractor("appid") { request =>
    request.getOptionalField[JsValue]("14") match {
      case Some(value: JsString) => Some(AppID(value.value, PLAIN))
      case _                     => None
    }
  }

  addExtractor("app_version") { request =>
    request.getOptionalField[JsValue]("16") match {
      case Some(value: JsString) => Some(AppVersion(value.value, PLAIN))
      case _                     => None
    }
  }

  addExtractor("idfv") {
    _.withOptionalFieldValue("27")(value =>
      LocalOSAdvertisingIdentifier(value, PLAIN))
  }

  addExtractor("idfa") {
    _.withOptionalFieldValue("19") { value =>
      GlobalOSAdvertisingIdentifier(value, PLAIN)
    }
  }

  addExtractor("os") { request =>
    val os: String = request.getOptionalField[JsString]("8") match {
      case Some(value) => value.value
      case None        => ""
    }
    val osv: String = request.getOptionalField[JsString]("9") match {
      case Some(value) => value.value
      case None        => ""
    }
    val str = os + osv
    if (str.nonEmpty) {
      Some(OS(str, PLAIN))
    } else {
      None
    }
  }

}
