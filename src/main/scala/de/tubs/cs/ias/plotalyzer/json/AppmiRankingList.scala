package de.tubs.cs.ias.plotalyzer.json

import de.tubs.cs.ias.plotalyzer.json.JSONReader.{
  appmiRankingListFormat,
  binaryAppEntryFormat
}
import spray.json.{JsNumber, JsObject, JsString, JsValue, JsonParser}
import wvlet.log.LogSupport

import java.io.File
import scala.io.Source

case class AppMetaData(bundleId: String,
                       version: String,
                       os: String,
                       categories: Map[String, Int])
    extends LogSupport {

  def getId: String = s"$bundleId:$version@$os"

  def toJson: JsValue = {
    if (categories.isEmpty) {
      error(s"apps $bundleId has no category...")
      JsObject()
    } else {
      val bestRank = categories.values.min
      JsObject(
        "bundleId" -> JsString(bundleId),
        "version" -> JsString(version),
        "os" -> JsString(os),
        "bestRank" -> JsNumber(bestRank),
        "bestCategory" -> {
          JsString(categories.toList.sortBy(_._1).find(_._2 == bestRank).get._1)
        },
        "categories" -> JsObject(
          categories.map(elem => elem._1 -> JsNumber(elem._2)))
      )
    }
  }

}

case class AppEntry(bundleId: String,
                    category: String,
                    name: String,
                    price: String,
                    rank: Int)

case class AppmiRankingList(apps: List[AppEntry])

object AppmiRankingListSet {

  def getAppSummary(rankingsFolder: String,
                    manifest: String): List[AppMetaData] = {
    val files = new File(rankingsFolder)
      .listFiles()
      .filter(_.isFile)
      .filter(_.getPath.endsWith(".json"))
      .map(_.getAbsolutePath)
      .toList
    getAppSummary(files, manifest)
  }

  private def readFile(file: String): String = {
    val source = Source.fromFile(file)
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  def getAppSummary(rankings: List[String],
                    manifest: String): List[AppMetaData] = {
    val demRankings: List[AppmiRankingList] = rankings.map { file =>
      JsonParser(readFile(file)).convertTo[AppmiRankingList]
    }
    val manifestJson: Map[String, BinaryAppEntry] =
      JsonParser(readFile(manifest)).asJsObject.fields.map {
        case (key, value) => key -> value.asJsObject.convertTo[BinaryAppEntry]
      }
    manifestJson.map {
      case (_, binaryAppEntry) =>
        val binaryName = binaryAppEntry.getBinaryName
        val rankingEntries =
          demRankings.flatMap(_.apps.find(_.bundleId == binaryName))
        AppMetaData(
          binaryAppEntry.id,
          binaryAppEntry.version,
          binaryAppEntry.os,
          rankingEntries.map(elem => elem.category -> elem.rank).toMap
        )
    }.toList
  }

}

case class BinaryAppEntry(id: String,
                          os: String,
                          path: String,
                          version: String) {

  def getBinaryName: String = {
    val name = path.split("/").last
    if (name.endsWith(".apk")) {
      val explode = name.replace(".apk", "").split('-').toList
      explode.slice(0, explode.length - 1).mkString("-")
    } else if (name.endsWith(".ipa")) {
      name.replace(".ipa", "")
    } else {
      throw new RuntimeException(s"häh!?! $path")
    }
  }

}
