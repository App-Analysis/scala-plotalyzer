package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

case class AppPreferences(id: Int,
                          analysis: Int,
                          interface: Option[Int],
                          app: MobileApp,
                          comment: String,
                          prefs: String)

object AppPreferences {

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
