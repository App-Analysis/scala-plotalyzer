package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.database.entities.trafficcollection.TrafficDirection.{
  TrafficDirection,
  REQUEST,
  RESPONSE
}
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

/** a trailer of a request
  *
  * @author Simon Koch
  *
  * @param name the name of the trailer
  * @param values the value(s) of the trailer
  */
case class Trailer(name: String, values: String)

/** companion object
  *
  * @author Simon Koch
  *
  */
object Trailer {

  /** get all trailers for requests
    *
    * @param id the list of requests/responses
    * @param database the database connection
    * @return the list of trailers
    */
  def get(id: List[Int], direction: TrafficDirection)(
      implicit database: Database): Map[Int, List[Trailer]] = {
    database.withDatabaseSession { implicit session =>
      id.grouped(10000)
        .flatMap { requests =>
          val query = direction match {
            case REQUEST =>
              sql"""SELECT request, name, values FROM Trailer WHERE request IN ($requests)"""
            case RESPONSE =>
              sql"""SELECT response, name, values FROM ResponseTrailer WHERE response IN ($requests)"""
          }
          query
            .map { entity =>
              entity.int(if (direction == REQUEST) "request" else "response") -> Trailer(
                entity.string("name"),
                entity.string("values"))
            }
            .toList
            .apply()
        }
        .toList
        .groupBy(_._1)
        .map(elem => elem._1 -> elem._2.map(_._2))
    }
  }
}
