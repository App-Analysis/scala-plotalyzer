package de.halcony.plotalyzer.plugins

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.database.entities.Experiment

/** AnalysisContext trait as basis for providing a plugin with context data
  *
  * @author Simon Koch
  */
trait AnalysisContext {

  /** the experiment to analyze, provides access to the stored data
    */
  val experiment: Experiment

  /** the app ids to restrict the analysis to
    */
  val only: Option[Seq[String]]

  /** the remaining cmd arguments after parsing the one provided to plotalyzer
    */
  val args: String

  /** the active database connection
    */
  val database: Database

  /** the plugin manager itself, in case there are cross dependencies between plugins
    */
  val plugins: PluginManager.type = PluginManager
}

/** Direct implementation of the AnalysisContext trait
  *
  * @param experiment the experiment to analyze
  * @param only the apps to restrict the analysis to
  * @param args the remaining cmd arguments
  */
case class BasicContext(override val experiment: Experiment,
                        override val only: Option[Seq[String]],
                        override val args: String,
                        override val database: Database)
    extends AnalysisContext
