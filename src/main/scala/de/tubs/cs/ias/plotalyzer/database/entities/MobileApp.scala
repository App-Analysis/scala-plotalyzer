package de.tubs.cs.ias.plotalyzer.database.entities

import scalikejdbc.WrappedResultSet

/** A mobile app and its meta data
  *
  * @author Simon Koch
  *
  * @param id the id of the app (i.e., de.app.name)
  * @param version the version string of the app
  * @param os the operating system the app belongs to (Android or iOS)
  */
case class MobileApp(id: String, version: String, os: String) {

  override def toString: String = s"$id:$version@$os"

}

/** companion object for the mobile app
  *
  * @author Simon Koch
  *
  */
object MobileApp {

  /** converts an selected entity into a mobile app
    *
    * expects the columns app_id, app_version, app_os
    *
    * @param entity the entity containing the expected attributes
    * @return the mobile app
    */
  def apply(entity: WrappedResultSet): MobileApp = {
    MobileApp(
      entity.string("app_id"),
      entity.string("app_version"),
      entity.string("app_os"),
    )
  }

}
