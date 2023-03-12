package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.utility.output.{StackTrace, Time}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.{JsNumber, JsObject, JsString, JsValue}
import java.time.ZonedDateTime

class ExperimentError(id: Int,
                      time: ZonedDateTime,
                      message: String,
                      stackTrace: String) {

  def getId: Int = id
  def getTime: ZonedDateTime = time
  def getMessage: String = message
  def getStackTrace: StackTrace = StackTrace(stackTrace)

  def toJson: JsValue = {
    JsObject(
      "id" -> JsNumber(getId),
      "time" -> JsString(Time.format(getTime)),
      "message" -> JsString(getMessage),
      "cause" -> (getStackTrace.getFirst("de.tubs.".r) match {
        case Some(hit) => JsString(hit)
        case None      => JsString(getStackTrace.trace)
      })
    )
  }

}

object ExperimentError {

  def apply(entity: WrappedResultSet): ExperimentError = {
    new ExperimentError(
      entity.int("id"),
      entity.zonedDateTime("time"),
      entity.string("message"),
      entity.string("stacktrace")
    )
  }

  def getExperimentErrors(experiment: Int)(
      implicit database: Database): List[ExperimentError] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     time,
                     experiment,
                     message,
                     stacktrace
              FROM experimenterror
              WHERE experiment = $experiment"""
        .map(ExperimentError.apply)
        .toList
        .apply()
    }
  }

  def getExperimentError(id: Int)(
      implicit database: Database): ExperimentError = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     time,
                     experiment,
                     message,
                     stacktrace
              FROM experimenterror
              WHERE id = $id
           """
        .map(ExperimentError.apply)
        .first
        .apply()
        .getOrElse(throw new RuntimeException(
          s"there is no experiment error with id $id"))
    }
  }

}
