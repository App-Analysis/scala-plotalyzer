package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

import java.time.ZonedDateTime

case class Response(id: Int,
                    startTime: ZonedDateTime,
                    httpVersion: String,
                    statusCode: Int,
                    reason: String,
                    contentRaw: Array[Byte],
                    content: String,
                    error: Option[String],
                    cookies: List[Cookie],
                    header: List[Header],
                    trailer: List[Trailer])

object Response {

  private case class Fragment(id: Int,
                              run: Int,
                              request: Int,
                              startTime: ZonedDateTime,
                              httpVersion: String,
                              statusCode: Int,
                              reason: String,
                              contentRaw: Array[Byte],
                              content: String,
                              error: Option[String])

  private object Fragment {

    def apply(entity: WrappedResultSet): Fragment = {
      Fragment(
        entity.int("id"),
        entity.int("run"),
        entity.int("request"),
        entity.zonedDateTime("start_time"),
        entity.string("http_version"),
        entity.int("status_code"),
        entity.string("reason"),
        entity.bytes("content_raw"),
        entity.string("content"),
        entity.stringOpt("error")
      )
    }

  }

  private def apply(fragment: Fragment,
                    cookies: List[Cookie],
                    headers: List[Header],
                    trailers: List[Trailer]): Response = {
    Response(
      fragment.id,
      fragment.startTime,
      fragment.httpVersion,
      fragment.statusCode,
      fragment.reason,
      fragment.contentRaw,
      fragment.content,
      fragment.error,
      cookies,
      headers,
      trailers
    )
  }

  def getResponseTable(requests: List[Int])(
      implicit database: Database
  ): Map[Int, List[Response]] = {
    val fragments: Map[Int, List[Fragment]] = database.withDatabaseSession {
      implicit session =>
        requests
          .grouped(10000)
          .flatMap { requestsPart =>
            sql"""SELECT id, run, request, start_time, http_version, status_code, reason, content_Raw, content, error
              FROM response
              WHERE request IN ($requestsPart)"""
              .map { entity: WrappedResultSet =>
                Fragment(entity)
              }
              .toList
              .apply()
              .groupBy(_.request)
          }
          .toMap
    }
    val responseIds = fragments.flatMap(_._2.map(_.id)).toList
    val cookies: Map[Int, List[Cookie]] =
      Cookie.get(responseIds, TrafficDirection.RESPONSE)
    val headers: Map[Int, List[Header]] =
      Header.get(responseIds, TrafficDirection.RESPONSE)
    val trailers: Map[Int, List[Trailer]] =
      Trailer.get(responseIds, TrafficDirection.RESPONSE)
    fragments.map {
      case (requestID, fragments) =>
        requestID -> fragments.map { frag =>
          Response(frag,
                   cookies.getOrElse(frag.id, List()),
                   headers.getOrElse(frag.id, List()),
                   trailers.getOrElse(frag.id, List()))
        }
    }
  }

}
