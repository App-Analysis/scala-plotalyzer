package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.utility.Picturesque
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

import java.awt.image.BufferedImage

class InterfaceElement(id: Int,
                       belongsTo: Int,
                       text: String,
                       clickable: Boolean)(implicit database: Database) {

  def getId: Int = id
  def getBelongsTo: Int = belongsTo
  def getText: String = text.toLowerCase.trim

  def getOriginalText: String = text

  def getClickable: Boolean = clickable

  def getScreenshot: Option[BufferedImage] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT screenshot FROM interfaceelement WHERE id = $id AND screenshot IS NOT NULL"""
        .map { entity =>
          Picturesque.getBufferedImage(entity.bytes("screenshot"))
        }
        .first
        .apply()
    }
  }

  def hasScreenshot: Boolean = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT TRUE AS succ FROM interfaceelement WHERE id = $id AND screenshot IS NOT NULL"""
        .map(_.boolean("succ"))
        .first
        .apply()
        .getOrElse(false)
    }
  }

}

object InterfaceElement {

  def apply(entity: WrappedResultSet)(
      implicit database: Database): InterfaceElement = {
    new InterfaceElement(
      entity.int("id"),
      entity.int("belongs_to"),
      entity.string("text"),
      entity.boolean("clickable")
    )
  }

  def get(interface: Int)(
      implicit database: Database): List[InterfaceElement] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                       belongs_to,
                       text,
                       clickable
                 FROM interfaceelement
                 WHERE belongs_to = $interface
             """.map(InterfaceElement.apply).toList.apply()
    }
  }
}
