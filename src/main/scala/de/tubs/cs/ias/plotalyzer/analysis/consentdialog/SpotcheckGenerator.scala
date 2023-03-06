package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import spray.json.{JsObject, JsString}
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Random

object SpotcheckGenerator extends LogSupport {

  def getTargetIds(potentialApps: List[String],
                   apps: List[String],
                   fill: Option[Int]): List[String] = {
    fill match {
      case Some(amount) =>
        val potentials: MSet[String] = MSet(potentialApps: _*)
        val selected: MSet[String] = MSet(apps: _*)
        selected.foreach(potentials.remove)
        while (selected.size < amount) {
          val gimmeList = potentials.toList
          val index = Random.nextInt(gimmeList.length)
          selected.addOne(gimmeList(index))
          potentials.remove(gimmeList(index))
        }
        info(s"reached required ${selected.size} ($amount) apps")
        selected.toList
      case None => apps
    }
  }

  def generate(analysis: ConsentDialogAnalysis,
               apps: List[String],
               fill: Option[Int],
               folder: String): Unit = {
    val targets = getTargetIds(
      analysis.getFinishedAnalysis.map(_.getApp.toString),
      apps,
      fill)
    val file = new File(folder)
    if (!file.exists || file.isDirectory) {
      file.mkdirs()
      val checkMap: MMap[String, JsObject] = MMap()
      targets.foreach { appId =>
        analysis.getFinishedAnalysis.find(_.getApp.toString == appId) match {
          case Some(appConsentAnalysis) =>
            checkMap.addOne(
              appId -> JsObject("classification" -> JsString(
                                  appConsentAnalysis.getConsentDialogType),
                                "correction" -> JsString("")))
            appConsentAnalysis.getInterfaceAnalysis.foreach { ia =>
              ia.dumpScreenshot(file.getAbsolutePath + "/" + appId + "/")
            }
          case None =>
            throw new RuntimeException(
              s"cannot spot check $appId as it does not exist")
        }

      }
      val checklist = new FileWriter(
        new File(file.getAbsolutePath + "/checklist.json"))
      try {
        checklist.write(JsObject(checkMap.toMap).prettyPrint)
      } finally {
        checklist.flush()
        checklist.close()
      }
    } else {
      throw new RuntimeException(
        "you have to provide a folder path that either does not exist or is a directory")
    }
  }

}
