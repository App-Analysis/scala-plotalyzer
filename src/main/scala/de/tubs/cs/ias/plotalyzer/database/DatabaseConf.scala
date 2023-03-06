package de.tubs.cs.ias.plotalyzer.database

import spray.json.{JsString, JsonParser}

import scala.io.Source

case class DatabaseConf(host: String,
                        port: String,
                        user: String,
                        pwd: String,
                        database: String)

object DatabaseConf {

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
