package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

case class Cookie(name: String, values: String)

object Cookie {

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
