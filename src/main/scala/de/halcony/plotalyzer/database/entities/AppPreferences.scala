package de.halcony.plotalyzer.database.entities

import de.halcony.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

/** App Preferences, i.e., stored key/value strings of an app
  *
  * @author Simon Koch
  *
  * @param id the id of the app preference json
  * @param analysis the corresponding analysis id
  * @param interface the corresponding interface
  * @param app the corresponding app
  * @param comment a corresponding comment
  * @param prefs the preferences string (json format)
  */
case class AppPreferences(id: Int,
                          analysis: Int,
                          interface: Option[Int],
                          app: MobileApp,
                          comment: String,
                          prefs: String)

/** companion object
  *
  * @author Simon Koch
  *
  */
object AppPreferences {

  /** convert a database entity to an object
    *
    * expects the columns, id, analysis, interface, app ..., comment, prefs
    *
    * @param entity the entity to convert
    * @return the converted entity
    */
  def apply(entity: WrappedResultSet): AppPreferences = {
    AppPreferences(
      entity.int("id"),
      entity.int("analysis"),
      entity.intOpt("interface"),
      MobileApp(entity),
      entity.string("comment"),
      entity.string("prefs")
    )
  }

  /** get all app preferences of a given interface analysis
    *
    * @param interfaceAnalysis the corresponding interface analysis
    * @param database the database connection
    * @return the retrieved app preferences
    */
  def get(interfaceAnalysis: InterfaceAnalysis)(
      implicit database: Database): List[AppPreferences] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id, analysis, interface, appid AS app_id, version AS app_version, os AS app_os, comment, prefs
              FROM apppreferences
              WHERE analysis = ${interfaceAnalysis.getId}
              ORDER BY id ASC
           """
        .map(AppPreferences.apply)
        .toList
        .apply()
    }
  }

}
