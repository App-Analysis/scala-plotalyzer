package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

/** An interaction with an element of an interface
  *
  * @author Simon Koch
  *
  * todo: this could be a case class as it is not mutable
  *
  * @param id the id of the interaction
  * @param action the action performed
  * @param onElement the id of the element interacted with
  * @param leadingTo the interface this action lead to
  */
class InterfaceElementInteraction(id: Int,
                                  action: String,
                                  onElement: Int,
                                  leadingTo: Option[Int]) {

  def getId: Int = id
  def getAction: String = action
  def getOnElement: Int = onElement
  def getLeadingTo: Option[Int] = leadingTo

}

/** companion object
  *
  * @author Simon Koch
  *
  */
object InterfaceElementInteraction {

  /** create an interface element interaction from an database entity
    *
    * expects the columns id, action, on_element, leading_to
    *
    * @param entity the entity retrieved from the database
    * @return the interface element interaction
    */
  def apply(entity: WrappedResultSet): InterfaceElementInteraction = {
    new InterfaceElementInteraction(
      entity.int("id"),
      entity.string("action"),
      entity.int("on_element"),
      entity.intOpt("leading_to")
    )
  }

  /** extract the element interaction of the given interface
    *
    * @param interface the interface for which to extract the possible interaction
    * @param database the database connection
    * @return if exists the interface element interaction else none
    */
  def get(interface: Interface)(
      implicit database: Database): Option[InterfaceElementInteraction] = {
    // element ids are unique per interface, i.e., no shared elements between interfaces
    val elementIds: Seq[Int] = interface.getInterfaceElements.map(_.getId)
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
