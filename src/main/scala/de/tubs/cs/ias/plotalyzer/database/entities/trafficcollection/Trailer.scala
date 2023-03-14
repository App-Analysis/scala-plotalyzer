package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
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
    * @param requests the list of requests
    * @param database the database connection
    * @return the list of trailers
    */
  def get(requests: List[Int])(
      implicit database: Database): Map[Int, List[Trailer]] = {
    database.withDatabaseSession { implicit session =>
      requests
        .grouped(10000)
        .flatMap { requests =>
          sql"""SELECT request, name, values FROM Trailer WHERE request IN ($requests)"""
            .map { entity =>
              entity.int("request") -> Trailer(entity.string("name"),
                                               entity.string("value"))
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
