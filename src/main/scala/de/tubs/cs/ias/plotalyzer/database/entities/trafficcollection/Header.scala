package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef

case class Header(name: String, values: String)

object Header {

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
