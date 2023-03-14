package de.halcony.plotalyzer

import de.halcony.plotalyzer.plugins.{PluginManagerConfiguration, RemotePluginConfig}
import spray.json.{DefaultJsonProtocol, JsonParser, RootJsonFormat}

import scala.io.Source

/** the object representation of the config.json
  *
  * @param plugin the plugin specific configuration
  */
case class Configuration(plugin: PluginManagerConfiguration)

object Configuration extends DefaultJsonProtocol {

  implicit val pluginFormat: RootJsonFormat[RemotePluginConfig] = jsonFormat2(
    RemotePluginConfig)

  implicit val pluginManagerConfigurationFormat
    : RootJsonFormat[PluginManagerConfiguration] = jsonFormat2(
    PluginManagerConfiguration)

  implicit val configurationFormat: RootJsonFormat[Configuration] = jsonFormat1(
    Configuration.apply)

  def read(file: String): Configuration = {
    val source = Source.fromFile(file)
    try {
      JsonParser(source.mkString).convertTo[Configuration]
    } finally {
      source.close()
    }
  }

}
