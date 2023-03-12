package de.tubs.cs.ias.plotalyzer

import de.halcony.argparse.{
  OptionalValue,
  Parser,
  ParsingException,
  ParsingResult
}
import de.tubs.cs.ias.plotalyzer.analysis.consentdialog.{
  ConsentDialogAnalysis,
  SpotcheckGenerator
}
import de.tubs.cs.ias.plotalyzer.analysis.tcfdetection.{
  DynamicTCFDetection,
  StaticTCFDetection,
  TCFDetectionConf
}
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.HostPathFilterTypes.{
  EXODUS,
  GENERIC
}
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.TrafficFilter
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.{
  RequestTrackingEndpointAnalysis,
  TrafficCollectionAnalysisConfig,
  TrafficSummary
}
import de.tubs.cs.ias.plotalyzer.analysis.{
  BasicExperimentSummary,
  ExperimentErrorSummary,
  FailedAnalysisSummary
}
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis
}
import de.tubs.cs.ias.plotalyzer.database.{Database, DatabaseConf}
import de.tubs.cs.ias.plotalyzer.json.AppmiRankingListSet
import de.tubs.cs.ias.plotalyzer.plugins.PluginManager
import de.tubs.cs.ias.plotalyzer.utility.Time
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue, JsonParser}
import wvlet.log.LogSupport

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object Plotalyzer extends LogSupport {

  val parser: Parser = Parser(
    "plotalyzer",
    "takes a scala-appanalyzer database and generates json for further processing")
    .addPositional("database-conf", "a json containing the database conf")
    .addPositional("id", "the id of the experiment to analyze")
    .addPositional(
      "out",
      "the path to print the result into (folder for screenshot json for data)")
    .addPositional("plugin", "the name of the plugin to use")
    .addDefault[ParsingResult => JsValue]("func", runPlugin, "run dem plugin")

  def runPlugin(pargs: ParsingResult): JsValue = {
    println(PluginManager.getPlugins)
    PluginManager.loadPlugins(pargs.getValue[String]("plugin")) match {
      case Some(plugin) =>
        plugin.analyze(null) match {
          case Left(value: Exception) => throw value
          case Right(value: JsValue)  => value
        }
      case None => JsObject("error" -> JsString("plugin cannot be loaded"))
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val pargs = parser.parse(args)
      val ret: Option[JsValue] = Option(
        pargs.getValue[ParsingResult => JsValue]("func")(pargs))
      ret match {
        case Some(value) =>
          val out = new BufferedWriter(
            new FileWriter(new File(pargs.getValue[String]("out"))))
          try {
            out.write(value.prettyPrint)
          } finally {
            out.flush()
            out.close()
          }
        case None =>
      }
    } catch {
      case x: ParsingException => println(x.getMessage())
    }
  }

  private def getDatabaseConnection(pargs: ParsingResult): Database = {
    val conf = DatabaseConf.read(pargs.getValue[String]("database-conf"))
    new Database(conf.host, conf.port, conf.user, conf.pwd, conf.database)
  }

}
