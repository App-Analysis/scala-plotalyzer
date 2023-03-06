package de.tubs.cs.ias.plotalyzer.database.entities

import scalikejdbc.WrappedResultSet

case class MobileApp(id: String, version: String, os: String) {

  override def toString: String = s"$id:$version@$os"

}

object MobileApp {

  /** converts an selected entity into a mobile app
    *
    * expects the columns app_id, app_version, app_os
    *
    * @param entity the entity containing the expected attributes
    * @return
    */
  def apply(entity: WrappedResultSet): MobileApp = {
    MobileApp(
      entity.string("app_id"),
      entity.string("app_version"),
      entity.string("app_os"),
    )
  }

}
