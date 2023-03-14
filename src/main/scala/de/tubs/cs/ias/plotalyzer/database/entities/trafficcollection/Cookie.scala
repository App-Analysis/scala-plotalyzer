package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
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
    * @param requests list of request ids
    * @param database the database connection
    * @return a mapping of request id to cookie list
    */
  def get(requests: List[Int])(
      implicit database: Database): Map[Int, List[Cookie]] = {
    database.withDatabaseSession { implicit session =>
      requests
        .grouped(10000)
        .flatMap { requests =>
          sql"""SELECT request, name, values FROM Cookie WHERE request IN ($requests)"""
            .map { entity =>
              entity.int("request") -> Cookie(entity.string("name"),
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
