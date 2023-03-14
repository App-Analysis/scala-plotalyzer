package de.halcony.plotalyzer.database.entities

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.utility.output.{StackTrace, Time}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.{JsNumber, JsObject, JsString, JsValue}

import java.time.ZonedDateTime

/** An error that occurred during the experiment (but not specific to an interface analysis)
  *
  * @author Simon Koch
  *
  * @param id the id of the error
  * @param time the time it occurred
  * @param message the message of the error
  * @param stackTrace the stacktrace of the error
  */
class ExperimentError(id: Int,
                      time: ZonedDateTime,
                      message: String,
                      stackTrace: String) {

  def getId: Int = id
  def getTime: ZonedDateTime = time
  def getMessage: String = message
  def getStackTrace: StackTrace = StackTrace(stackTrace)

  /** convert the error to a json representation
    *
    * @return a JsObject
    */
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

/** companion object
  *
  */
object ExperimentError {

  /** convert a retrieved entity to an experiment error
    *
    * @param entity the entity to convert
    * @return the corresponding experiment error
    */
  def apply(entity: WrappedResultSet): ExperimentError = {
    new ExperimentError(
      entity.int("id"),
      entity.zonedDateTime("time"),
      entity.string("message"),
      entity.string("stacktrace")
    )
  }

  /** get all errors of an experiment
    *
    * @param experiment the id of the experiment
    * @param database the database connection
    * @return
    */
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

  /** get a specific experiment error
    *
    * @param id the id of the experiment error
    * @param database the database connection
    * @return the experiment error
    */
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
