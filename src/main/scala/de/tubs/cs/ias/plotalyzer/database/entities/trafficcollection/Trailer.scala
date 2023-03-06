package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

case class Trailer(name: String, values: String)

object Trailer {
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
