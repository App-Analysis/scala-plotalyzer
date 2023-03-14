package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

import java.time.ZonedDateTime

/** a traffic collection
  *
  * @author Simon Koch
  * todo: could be a case class as it is not mutable
  *
  * @param id the id of the traffic collection
  * @param analysis the id of the corresponding analysis
  * @param interface the id of the corresponding interface
  * @param start the start of the traffic collection
  * @param end the end of the traffic collection
  * @param comment the comment stored for the traffic collection
  * @param requests the requests of the traffic collection
  */
class TrafficCollection(id: Int,
                        analysis: Int,
                        interface: Option[Int],
                        start: ZonedDateTime,
                        end: ZonedDateTime,
                        comment: String,
                        requests: List[Request]) {

  def getId: Int = id
  def getAnalysis: Int = analysis
  def getInterface: Option[Int] = interface
  def getStart: ZonedDateTime = start
  def getEnd: ZonedDateTime = end
  def getComment: String = comment
  def getRequests: List[Request] = requests

}

/** companion object
  *
  * @author Simon Koch
  *
  */
object TrafficCollection {

  /** a prototype of a traffic collection lacking the actual requests
    *
    * @param id the id of the traffic collection
    * @param analysis the id of the analysis
    * @param interface the id of the interface
    * @param start the start of the traffic collection
    * @param end the end of the traffic collection
    * @param comment the stored comment of the traffic collection
    */
  private case class TrafficCollectionPrototype(id: Int,
                                                analysis: Int,
                                                interface: Option[Int],
                                                start: ZonedDateTime,
                                                end: ZonedDateTime,
                                                comment: String)

  def getTrafficCollections(experiment: Int)(
      implicit database: Database): List[TrafficCollection] = {
    database.withDatabaseSession { implicit session =>
      val analysis =
        sql"""SELECT id FROM interfaceanalysis WHERE experiment = $experiment"""
          .map(_.int("id"))
          .toList
          .apply()
      val collections =
        sql"""SELECT id,
                       analysis,
                       interface,
                       start,
                       stop,
                       comment
                FROM trafficcollection
                WHERE analysis IN ($analysis)
             """
          .map { entity =>
            TrafficCollectionPrototype(
              entity.int("id"),
              entity.int("analysis"),
              entity.intOpt("interface"),
              entity.zonedDateTime("start"),
              entity.zonedDateTime("stop"),
              entity.string("comment")
            )
          }
          .toList
          .apply()
      val requestTable = Request.getRequestTable(collections.map(_.id))
      collections.map { prototype =>
        new TrafficCollection(
          prototype.id,
          prototype.analysis,
          prototype.interface,
          prototype.start,
          prototype.end,
          prototype.comment,
          requestTable.getOrElse(prototype.id, List())
        )
      }
    }
  }
}
