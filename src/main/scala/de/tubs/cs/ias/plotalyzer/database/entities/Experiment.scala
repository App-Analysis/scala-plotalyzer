package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import java.time.ZonedDateTime

class Experiment(id: Int, description: String, created: ZonedDateTime)(
    implicit database: Database) {

  def getId: Int = id
  def getDescription: String = description
  def getCreated: ZonedDateTime = created

  def getExperimentErrors: List[ExperimentError] =
    ExperimentError.getExperimentErrors(this.getId)

  def getInterfaceAnalysisIds: List[Int] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id FROM interfaceanalysis WHERE experiment = ${this.getId}"""
        .map(_.int("id"))
        .toList
        .apply()
    }
  }
}

object Experiment {

  def apply(entity: WrappedResultSet)(
      implicit database: Database): Experiment = {
    new Experiment(
      entity.int("id"),
      entity.string("description"),
      entity.zonedDateTime("created")
    )
  }

  def apply(id: String)(implicit database: Database): Experiment = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id, description, created
            FROM experiment
            WHERE id = ${id.toInt}
         """
        .map(Experiment.apply)
        .first
        .apply()
        .getOrElse(
          throw new RuntimeException(s"there is no experiment with id $id"))
    }
  }

}
