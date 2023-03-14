package de.tubs.cs.ias.plotalyzer.database

import scalikejdbc.{
  ConnectionPool,
  ConnectionPoolSettings,
  DB,
  DBSession,
  using
}

/** class representing an established database connection
  *
  * @author Simon Koch
  *
  * @param host the database host
  * @param port the database port
  * @param user the database user
  * @param pwd  the database user password
  * @param database the database name
  */
class Database(host: String,
               port: String,
               user: String,
               pwd: String,
               database: String) {

  private var poolSet: Boolean = false
  private val POOL_NAME = "appanalyzer"

  initializeConnectionPool()

  /** initializes the connection pool
    *
    */
  private def initializeConnectionPool(): Unit = {
    if (!poolSet) {
      val settings: ConnectionPoolSettings = ConnectionPoolSettings(
        initialSize = 10,
        maxSize = 10,
        driverName = "org.postgresql.Driver")
      val url =
        s"jdbc:postgresql://$host:$port/$database"
      ConnectionPool.add(POOL_NAME, url, user, pwd, settings)
      poolSet = true
    }
  }

  /** run function with an open database session and close session after
    *
    * @param func the function to run with an open db connection
    * @tparam T the return type of the function
    * @return the return value of the function
    */
  def withDatabaseSession[T](func: DBSession => T): T = {
    if (poolSet) {
      using(ConnectionPool(POOL_NAME).borrow()) { con =>
        DB(con).localTx { session =>
          func(session)
        }
      }
    } else {
      throw new RuntimeException(
        "there is no postgres connection pool, initialize first")
    }
  }

}
