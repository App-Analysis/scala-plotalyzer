package de.tubs.cs.ias.plotalyzer.plugins

case class RemotePluginConfig(owner: String, repo: String)

case class PluginManagerConfiguration(
    folder: String,
    available: Map[String, RemotePluginConfig])
