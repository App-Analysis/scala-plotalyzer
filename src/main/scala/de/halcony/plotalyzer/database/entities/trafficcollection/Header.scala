package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.database.entities.trafficcollection.TrafficDirection.{
  TrafficDirection,
  RESPONSE,
  REQUEST
}
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

/** an intercepted header of a request
  *
  * @author Simon Koch
  *
  * @param name the name of the header
  * @param values the value of the header
  */
case class Header(name: String, values: String)

/** companion object
  *
  * @author Simon Koch
  *
  */
object Header {

  /** get all headers belonging to requests
    *
    * @param ids the list of request/responses
    * @param database the database connection
    * @return a mapping of request id to list of headers
    */
  def get(ids: List[Int], direction: TrafficDirection)(
      implicit database: Database): Map[Int, List[Header]] = {
    database.withDatabaseSession { implicit session =>
      ids
        .grouped(10000)
        .flatMap { requests =>
          val query = direction match {
            case REQUEST =>
              sql"""SELECT request, name, values FROM Header WHERE request IN ($requests)"""
            case RESPONSE =>
              sql"""SELECT response, name, values FROM ResponseHeader WHERE response IN ($requests)"""
          }
          query
            .map { entity =>
              entity.int(if (direction == REQUEST) "request" else "response") -> Header(
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
