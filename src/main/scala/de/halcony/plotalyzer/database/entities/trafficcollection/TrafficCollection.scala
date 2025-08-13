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

  /** get the next chunk of apps for the cache starting from offset
   * 
   * @param experiment the experiment id
   * @param chunks the number of analysis to load (-1 means load all)
   * @param offset the offset after which we want to get the analysis
   * @return the app analysis ids we want to load in the cache
   */                                              
  def getChunkAnalysisIds(experiment: Int, chunks: Int, offset: Int)(
    implicit database: Database): List[Int] = {
    database.withDatabaseSession { implicit session =>
      if (chunks == -1) {
        sql"""SELECT id FROM interfaceanalysis WHERE experiment = $experiment"""
          .map(_.int("id"))
          .toList
          .apply()
      } else {
        sql"""SELECT id FROM interfaceanalysis WHERE experiment = $experiment 
              LIMIT $chunks OFFSET $offset"""
          .map(_.int("id"))
          .toList
          .apply()
      }
    }
  }
  
  def getTrafficCollections(experiment: Int, chunks: Int, offset: Int = 0)(
      implicit database: Database): List[TrafficCollection] = {
    database.withDatabaseSession { implicit session =>
      val analysis = getChunkAnalysisIds(experiment, chunks, offset)

      if (analysis.length == 0) {
        return List()
      }

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
