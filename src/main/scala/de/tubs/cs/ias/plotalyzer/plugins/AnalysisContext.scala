package de.tubs.cs.ias.plotalyzer.plugins

import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, MobileApp}

trait AnalysisContext {
  val experiment: Experiment
  val only: Option[Seq[MobileApp]]
  val args: String
  val plugins: PluginManager.type = PluginManager
}

case class BasicContext(override val experiment: Experiment,
                        override val only: Option[Seq[MobileApp]],
                        override val args: String)
    extends AnalysisContext
