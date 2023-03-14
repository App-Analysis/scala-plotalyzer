package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
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
    * @param requests the list of request ids
    * @param database the database connection
    * @return a mapping of request id to list of headers
    */
  def get(requests: List[Int])(
      implicit database: Database): Map[Int, List[Header]] = {
    database.withDatabaseSession { implicit session =>
      requests
        .grouped(10000)
        .flatMap { requests =>
          sql"""SELECT request, name, values FROM Header WHERE request IN ($requests)"""
            .map { entity =>
              entity.int("request") -> Header(entity.string("name"),
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
