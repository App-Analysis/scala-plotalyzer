package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.database.entities.trafficcollection.TrafficDirection.{
  RESPONSE,
  REQUEST,
  TrafficDirection
}
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

/** a cookie intercepted with a request
  *
  * @author Simon Koch
  *
  * @param name the name of the cookie
  * @param values the value(s) of the cookie
  */
case class Cookie(name: String, values: String)

/** companion object
  *
  */
object Cookie {

  /** get all cookies belonging to the request ids
    *
    * @param ids list of request ids
    * @param database the database connection
    * @return a mapping of request id to cookie list
    */
  def get(ids: List[Int], direction: TrafficDirection)(
      implicit database: Database): Map[Int, List[Cookie]] = {
    database.withDatabaseSession { implicit session =>
      ids
        .grouped(10000)
        .flatMap { requests =>
          val query = direction match {
            case REQUEST =>
              sql"""SELECT request, name, values FROM Cookie WHERE request IN ($requests)"""
            case RESPONSE =>
              sql"""SELECT response, name, values FROM ResponseCookie WHERE response IN ($requests)"""
          }
          query
            .map { entity =>
              entity.int(if (direction == REQUEST) "request" else "response") -> Cookie(
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
