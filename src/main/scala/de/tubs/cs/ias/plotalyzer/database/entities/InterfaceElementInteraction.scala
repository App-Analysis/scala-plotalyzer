package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

class InterfaceElementInteraction(id: Int,
                                  action: String,
                                  onElement: Int,
                                  leadingTo: Option[Int]) {

  def getId: Int = id
  def getAction: String = action
  def getOnElement: Int = onElement
  def getLeadingTo: Option[Int] = leadingTo

}

object InterfaceElementInteraction {

  def apply(entity: WrappedResultSet): InterfaceElementInteraction = {
    new InterfaceElementInteraction(
      entity.int("id"),
      entity.string("action"),
      entity.int("on_element"),
      entity.intOpt("leading_to")
    )
  }

  def get(analysis: Interface)(
      implicit database: Database): Option[InterfaceElementInteraction] = {
    val elementIds: Seq[Int] = analysis.getInterfaceElements.map(_.getId)
    if (elementIds.nonEmpty) {
      database.withDatabaseSession { implicit session =>
        sql"""SELECT id,
                     action,
                     on_element,
                     leading_to
              FROM interfaceelementinteraction
              WHERE on_element IN (${elementIds})
           """
          .map(InterfaceElementInteraction.apply)
          .first
          .apply()
      }
    } else {
      None
    }
  }

}
