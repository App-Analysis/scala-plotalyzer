package de.tubs.cs.ias.plotalyzer

import de.halcony.argparse.{
  OptionalValue,
  Parser,
  ParsingException,
  ParsingResult
}
import de.tubs.cs.ias.plotalyzer.database.entities.Experiment
import de.tubs.cs.ias.plotalyzer.database.{Database, DatabaseConf}
import de.tubs.cs.ias.plotalyzer.plugins.{
  AnalysisReturn,
  BasicContext,
  JSONReturn,
  PluginManager
}
import spray.json.{JsObject, JsString}
import wvlet.log.LogSupport
import java.io.File
import scala.io.Source

/** The main object handling running the plotalyzer and its plugins
  *
  * @author Simon Koch
  */
object Plotalyzer extends LogSupport {

  /** cmd parser spec
    */
  val parser: Parser = Parser(
    "plotalyzer",
    "takes a scala-appanalyzer database and generates json for further processing")
    .addOptional("conf",
                 "c",
                 "config",
                 Some("./resources/config.json"),
                 "the configuration file")
    .addSubparser(PluginManager.parser)
    .addSubparser(
      Parser("analysis", "perform an analysis")
        .addOptional("database-conf",
                     "d",
                     "database-conf",
                     Some("./resources/db.json"),
                     "a json containing the database conf")
        .addPositional("id", "the id of the experiment to analyze")
        .addPositional("out", "the path to print the result into")
        .addPositional("plugin", "the name of the plugin to use")
        .addOptional("only",
                     "o",
                     "only",
                     None,
                     "only analyze apps contained in the csv or file provided")
        .addDefault[(ParsingResult, Configuration) => Unit]("func",
                                                            runPlugin,
                                                            "run dem plugin"))

  /** based on the provided --only flags returns the set of app ids to restrict the analysis to
    *
    * @param str the string either pointing to a file in which each line is one app or the string itself is a csv list
    * @return the retrieved set of app ids to restrict the analysis to
    */
  private def getOnly(str: String): Seq[String] = {
    val file = new File(str)
    if (file.exists() && file.isFile) {
      val source = Source.fromFile(file)
      try {
        source.getLines().toSet.toSeq
      } finally {
        source.close()
      }
    } else {
      str.split(",").toSeq
    }
  }

  /** extracts cmd args, runs the specified plugin, and writes the results into out
    *
    * @param pargs the parsed command line arguments
    */
  def runPlugin(pargs: ParsingResult, conf: Configuration): Unit = {
    val pluginManager = PluginManager.getPluginManager(conf.plugin)
    pluginManager.loadPlugins(pargs.getValue[String]("plugin")) match {
      case Some(plugin) =>
        implicit val database: Database = getDatabaseConnection(pargs)
        val experiment: Experiment = Experiment(pargs.getValue[String]("id"))
        val only: Option[Seq[String]] =
          pargs.get[OptionalValue[String]]("only").value match {
            case Some(only) => Some(getOnly(only))
            case None       => None
          }
        val context = BasicContext(experiment, only, "") // create plugin analysis context
        plugin.analyze(context) match {
          case Left(value: Exception) => throw value
          case Right(ret: AnalysisReturn) =>
            ret.write(pargs.getValue[String]("out"))
        }
      // if there is no plugin to be loaded we write give an error log and write an error json
      case None =>
        error("no plugin loaded - something went wrong")
        JSONReturn(JsObject("error" -> JsString("plugin cannot be loaded")))
          .write(pargs.getValue[String]("out"))
    }
  }

  /** parses the command line args and starts the correct main
    *
    * @param args the cmd arguments
    */
  def main(args: Array[String]): Unit = {
    try {
      val pargs = parser.parse(args)
      val conf = Configuration.read(pargs.getValue[String]("conf"))
      pargs.getValue[(ParsingResult, Configuration) => Unit]("func")(pargs,
                                                                     conf)
    } catch {
      case x: ParsingException => println(x.getMessage())
    }
  }

  /** creates the database connection handler based on the cmd arguments
    *
    * @param pargs the parsed command line arguments
    * @return the database connection handler
    */
  private def getDatabaseConnection(pargs: ParsingResult): Database = {
    val conf = DatabaseConf.read(pargs.getValue[String]("database-conf"))
    new Database(conf.host, conf.port, conf.user, conf.pwd, conf.database)
  }

}
