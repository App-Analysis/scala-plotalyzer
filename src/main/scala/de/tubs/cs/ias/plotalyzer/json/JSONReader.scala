package de.tubs.cs.ias.plotalyzer.json

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

object JSONReader extends DefaultJsonProtocol {

  implicit val appEntryFormat: RootJsonFormat[AppEntry] = jsonFormat5(AppEntry)

  implicit val appmiRankingListFormat: RootJsonFormat[AppmiRankingList] =
    jsonFormat1(AppmiRankingList)

  implicit val binaryAppEntryFormat: RootJsonFormat[BinaryAppEntry] =
    jsonFormat4(BinaryAppEntry)

}
