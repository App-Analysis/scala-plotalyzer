package de.tubs.cs.ias.plotalyzer.database

import scalikejdbc.{
  ConnectionPool,
  ConnectionPoolSettings,
  DB,
  DBSession,
  using
}

class Database(host: String,
               port: String,
               user: String,
               pwd: String,
               database: String) {

  private var poolSet: Boolean = false
  private val POOL_NAME = "appanalyzer"

  initializeConnectionPool()

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
