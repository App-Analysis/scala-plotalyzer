package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import scalikejdbc.{
  DBSession,
  WrappedResultSet,
  scalikejdbcSQLInterpolationImplicitDef
}

import java.time.ZonedDateTime

/** a request intercepted during traffic collection
  *
  * @author Simon Koch
  *
  * @param id the id of the request
  * @param run the traffic collection run id
  * @param start the start of the request
  * @param scheme the scheme of the request
  * @param method the method of the request
  * @param host the host of the request
  * @param port the port of the request
  * @param path the path of the request
  * @param content the content of the request (string)
  * @param contentRaw the raw content of the request (bytes)
  * @param authority the authority of the request
  * @param error any errors encountered while intercepting the request
  * @param cookies cookies transmitted with the request
  * @param headers headers transmitted with the request
  * @param trailers trailers transmitted with the request
  */
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
                   trailers: List[Trailer],
                   response: List[Response]) {

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

/** companion object
  *
  * @author Simon Koch
  *
  */
object Request {

  /** a request fragment not yet containing cookies, headers, or trailers
    *
    * @author Simon Koch
    *
    * @param id the id of the request
    * @param run the run id of the traffic collection
    * @param start the start of the request
    * @param scheme the scheme of the request
    * @param method the method of the request
    * @param host the host of the request
    * @param port the port of the request
    * @param path the path of the request
    * @param content the content of the request (string)
    * @param contentRaw the content of the request raw (bytes)
    * @param authority the authority of the request
    * @param error the encountered errors while intercepting the request
    */
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

  /** companion object
    *
    * @author Simon Koch
    *
    */
  private object Fragment {

    /** convert an request fragment entity to an object
      *
      * expects the columns id, run, start_time, scheme, method, host, port, path, content, content_Raw, authority, error
      *
      * @param entity the entity of interest
      * @return the corresponding object
      */
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

  /** uses a fragment, list of cookies, list of headers, list of trailers, to create a proper request object
    *
    * @param fragment the fragment of the request
    * @param cookies the corresponding list of cookies
    * @param headers the corresponding list of headers
    * @param trailers the corresponding list of trailers
    * @return
    */
  private def apply(fragment: Fragment,
                    cookies: List[Cookie],
                    headers: List[Header],
                    trailers: List[Trailer],
                    responses: List[Response]): Request = {
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
      trailers,
      responses
    )
  }

  /** get a request table for a set of request collection runs
    *
    * this is basically an in-memory fast accessible copy of the subset of the request table for faster interaction
    * which highly improves access and processing times
    *
    * @param runs the runs of interest
    * @param database the database connection
    * @return a map mapping collection runs with list of requests
    */
  def getRequestTable(runs: List[Int])(
      implicit database: Database): Map[Int, List[Request]] = {
    val fragments: Map[Int, List[Fragment]] = database.withDatabaseSession {
      implicit session: DBSession =>
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
    val cookies: Map[Int, List[Cookie]] =
      Cookie.get(requestIds, TrafficDirection.REQUEST)
    val headers: Map[Int, List[Header]] =
      Header.get(requestIds, TrafficDirection.REQUEST)
    val trailers: Map[Int, List[Trailer]] =
      Trailer.get(requestIds, TrafficDirection.REQUEST)
    val responses: Map[Int, List[Response]] =
      Response.getResponseTable(requestIds)
    fragments.map {
      case (run, fragments) =>
        run -> fragments.map(
          frag =>
            Request(frag,
                    cookies.getOrElse(frag.id, List()),
                    headers.getOrElse(frag.id, List()),
                    trailers.getOrElse(frag.id, List()),
                    responses.getOrElse(frag.id, List())))
    }
  }

}
