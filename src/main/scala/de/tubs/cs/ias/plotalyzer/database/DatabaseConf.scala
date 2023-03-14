package de.tubs.cs.ias.plotalyzer.database

import spray.json.{JsString, JsonParser}
import scala.io.Source

/** Class holding the configuration parameter of the postgresql connection
  *
  *  @author Simon Koch
  *
  * @param host the database host
  * @param port the database port
  * @param user the database user
  * @param pwd  the database user password
  * @param database the database user name
  */
case class DatabaseConf(host: String,
                        port: String,
                        user: String,
                        pwd: String,
                        database: String)


/** companion object for the database configuration to read in the config from a json
  *
  * @author Simon Koch
  *
  */
object DatabaseConf {

  /** read in a configuration json for the database
    *
    * @param path the path to the configuration json
    * @return the parsed database configuration
    */
  def read(path: String): DatabaseConf = {
    val source = Source.fromFile(path)
    try {
      val fields = JsonParser(source.getLines().mkString("\n")).asJsObject.fields
      DatabaseConf(
        fields("host").asInstanceOf[JsString].value,
        fields("port").asInstanceOf[JsString].value,
        fields("user").asInstanceOf[JsString].value,
        fields("pwd").asInstanceOf[JsString].value,
        fields("database").asInstanceOf[JsString].value
      )
    } finally {
      source.close()
    }
  }

}
