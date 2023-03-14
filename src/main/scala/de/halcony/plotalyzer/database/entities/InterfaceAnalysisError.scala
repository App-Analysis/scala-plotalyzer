package de.halcony.plotalyzer.database.entities

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.utility.output.StackTrace
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.{JsNull, JsNumber, JsObject, JsString, JsValue}

/** an error that occurred during an interface analysis
  *
  * @author Simon Koch
  *
  * @param id the id of the error
  * @param analysis the corresponding analysis id
  * @param interface the interface the error occurred at
  * @param message the message of the error
  * @param stacktrace the corresponding stacktrace
  */
class InterfaceAnalysisError(id: Int,
                             analysis: Int,
                             interface: Option[Int],
                             message: String,
                             stacktrace: String) {

  def getId: Int = id
  def getAnalysisId: Int = analysis
  def getInterfaceId: Option[Int] = interface
  def getMessage: String = message
  def getStacktrace: StackTrace = StackTrace(stacktrace)

  /** convert the error to a JsObject
    *
    * @return the JsObject
    */
  def toJson: JsValue = {
    JsObject(
      "id" -> JsNumber(getId),
      "analysis" -> JsNumber(analysis),
      "interface" -> (getInterfaceId match {
        case Some(id) => JsNumber(id)
        case None     => JsNull
      }),
      "message" -> JsString(getMessage),
      "cause" -> (getStacktrace.getFirst("de.tubs".r) match {
        case Some(hit) => JsString(hit)
        case None      => JsString(getStacktrace.trace)
      })
    )
  }

}

/** the companion object
  *
  * @author Simon Koch
  *
  */
object InterfaceAnalysisError {

  /** extract an interface analysis error from an entity
    *
    * expects the columns id,analysis,interface,message,stacktrace
    *
    * @param entity the entity to convert
    * @return the extracted interface analysis error
    */
  def apply(entity: WrappedResultSet): InterfaceAnalysisError = {
    new InterfaceAnalysisError(
      entity.int("id"),
      entity.int("analysis"),
      entity.intOpt("interface"),
      entity.string("message"),
      entity.string("stacktrace")
    )
  }

  /** get errors related to the provided analysis ids
    *
    * @param analysis a list of analysis ids
    * @param database the database connection
    * @return a list of all errors
    */
  def getInterfaceAnalysisErrors(analysis: List[Int])(
      implicit database: Database): List[InterfaceAnalysisError] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     analysis,
                     interface,
                     message,
                     stacktrace
              FROM interfaceanalysiserror
              WHERE analysis IN ($analysis)
           """.map(InterfaceAnalysisError.apply).toList.apply()
    }
  }

  /** get a specific analysis error
    *
    * @param id the id of the error
    * @param database the database connection
    * @return the interface analysis error
    */
  def getInterfaceAnalysisError(id: Int)(
      implicit database: Database): InterfaceAnalysisError = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     analysis,
                     interface,
                     message,
                     stacktrace
              FROM interfaceanalysiserror
              WHERE id = $id
           """
        .map(InterfaceAnalysisError.apply)
        .first
        .apply()
        .getOrElse(throw new RuntimeException(
          s"there is no interface analysis error with id $id"))
    }
  }

}
