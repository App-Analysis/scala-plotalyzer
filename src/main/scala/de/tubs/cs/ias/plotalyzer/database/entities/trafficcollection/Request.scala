package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

import java.time.ZonedDateTime

case class Request(id: Int,
                   run: Int,
                   start: ZonedDateTime,
                   scheme: String,
                   method: String,
                   host: String,
                   port: String,
                   path: String,
                   content: String,
                   contentRaw: Array[Byte],
                   authority: String,
                   error: Option[String],
                   cookies: List[Cookie],
                   headers: List[Header],
                   trailers: List[Trailer]) {

  def getHost: String = Option(host) match {
    case Some(value) => value
    case None        => ""
  }

  def getPath: String = Option(path) match {
    case Some(value) => value.split('?').head
    case None        => ""
  }

  def getUrl: String = s"$scheme://$host$getPath"

  def getPathWithQuery = Option(path) match {
    case Some(value) => value
    case None        => ""
  }

  def getFullUrl: String = s"$scheme://$host$getPathWithQuery"

}

object Request {

  private case class Fragment(id: Int,
                              run: Int,
                              start: ZonedDateTime,
                              scheme: String,
                              method: String,
                              host: String,
                              port: String,
                              path: String,
                              content: String,
                              contentRaw: Array[Byte],
                              authority: String,
                              error: Option[String])

  private object Fragment {
    def apply(entity: WrappedResultSet): Fragment = {
      Fragment(
        entity.int("id"),
        entity.int("run"),
        entity.zonedDateTime("start_time"),
        entity.string("scheme"),
        entity.string("method"),
        entity.string("host"),
        entity.string("port"),
        entity.string("path"),
        entity.string("content"),
        entity.bytes("content_raw"),
        entity.string("authority"),
        entity.stringOpt("error")
      )
    }

  }

  private def apply(fragment: Fragment,
                    cookies: List[Cookie],
                    headers: List[Header],
                    trailers: List[Trailer]): Request = {
    Request(
      fragment.id,
      fragment.run,
      fragment.start,
      fragment.scheme,
      fragment.method,
      fragment.host,
      fragment.port,
      fragment.path,
      fragment.content,
      fragment.contentRaw,
      fragment.authority,
      fragment.error,
      cookies,
      headers,
      trailers
    )
  }

  def getRequestTable(runs: List[Int])(
      implicit database: Database): Map[Int, List[Request]] = {
    val fragments: Map[Int, List[Fragment]] = database.withDatabaseSession {
      implicit session =>
        sql"""SELECT id, run, start_time, scheme, method, host, port, path, content, content_raw, authority, error
              FROM request
              WHERE run IN ($runs)
           """
          .map { entity: WrappedResultSet =>
            Fragment(entity)
          }
          .toList
          .apply()
          .groupBy(_.run)
    }
    val requestIds = fragments.flatMap(_._2.map(_.id)).toList
    val cookies: Map[Int, List[Cookie]] = Cookie.get(requestIds)
    val headers: Map[Int, List[Header]] = Header.get(requestIds)
    val trailers: Map[Int, List[Trailer]] = Trailer.get(requestIds)
    fragments.map {
      case (run, fragments) =>
        run -> fragments.map(
          frag =>
            Request(frag,
                    cookies.getOrElse(frag.id, List()),
                    headers.getOrElse(frag.id, List()),
                    trailers.getOrElse(frag.id, List())))
    }
  }

}
