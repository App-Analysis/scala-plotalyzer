package de.halcony.plotalyzer.plugins

import de.halcony.argparse.{OptionalValue, Parser, ParsingResult}
import de.halcony.plotalyzer.{Configuration, Plotalyzer}
import org.clapper.classutil.{ClassFinder, ClassInfo}
import spray.json.{JsArray, JsonParser}
import wvlet.log.LogSupport

import java.io.{File, FileOutputStream}
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.net.{URI, URL, URLClassLoader}
import scala.annotation.{nowarn, tailrec}

/** The PluginManager responsible for finding and loading available plugins
  *
  * @author Simon Koch
  */
class PluginManager(conf: PluginManagerConfiguration) extends LogSupport {

  /** the directory in which to store the plugins
    * todo: move to config file
    */
  private val PLUGIN_DIRECTORY = conf.folder

  /** the fully qualified name of the AnalysisPlugin trait
    */
  private val PLUGIN_INTERFACE =
    "de.tubs.cs.ias.plotalyzer.plugins.AnalysisPlugin"

  /** the jars contained in the PLUGIN_DIRECTORY folder on startup
    */
  val jars: List[File] = new File(PLUGIN_DIRECTORY).listFiles
    .filter(_.getPath.endsWith(".jar"))
    .toList

  /** a map of the plugins and the name of the main classes
    *
    */
  private val pluginMap: Map[String, (ClassInfo, File)] = {
    jars.flatMap { jar =>
      val finder = ClassFinder(List(jar))
      finder.getClasses().filter(_.interfaces.contains(PLUGIN_INTERFACE)).map {
        info =>
          info.name.split("\\.").last -> (info, jar)
      }
    }.toMap
  }

  /** get all known plugins
    *
    * @return the set of class names of known plugins / classes implementing the AnalysisPlugin trait
    */
  def getInstalledPlugins: Set[(String, String)] =
    pluginMap.map {
      case (name, (_, file)) =>
        (name, file.getPath.split("-").last.stripSuffix(".jar"))
    }.toSet

  def removePlugin(name: String): Unit = {
    pluginMap(name)._2.delete()
  }

  /** loads a plugin specified by (class) name
    *
    * @param name the (class) name of the plugin to be loaded
    * @return the instantiated plugin
    */
  def loadPlugins(name: String): Option[AnalysisPlugin] = {
    val classInfo: (ClassInfo, File) = pluginMap.getOrElse(
      name,
      throw new IllegalArgumentException(s"the plugin $name does not exist"))
    val urls: List[URL] = List(classInfo._2.toURI.toURL)
    val parentClassLoader =
      Plotalyzer.getClass.getClassLoader
    val childClassLoader =
      URLClassLoader.newInstance(urls.toArray, parentClassLoader)
    try {
      Some(
        Class
          .forName(classInfo._1.name, true, childClassLoader)
          .newInstance()
          .asInstanceOf[AnalysisPlugin])
    } catch {
      case x: NoClassDefFoundError =>
        error(x)
        error(
          s"plugin $name cannot be loaded check the plugin directory and make sure it is contained")
        None
    } finally {
      childClassLoader.close()
    }
  }

}

/** companion object
  *
  * @author Simon Koch
  *
  */
object PluginManager extends LogSupport {

  private var manager: Option[PluginManager] = None

  /** get singleton instance of plugin manager
    *
    * @param configuration the plugin manager configuration
    * @return
    */
  @tailrec
  def getPluginManager(
      configuration: PluginManagerConfiguration): PluginManager =
    manager match {
      case Some(manager) => manager
      case None =>
        manager = Some(new PluginManager(configuration))
        getPluginManager(configuration)
    }

  /** cmd arg parser for the plugin manager
    */
  val parser: Parser = Parser("plugin", "manage the installed plugins")
    .addSubparser(
      Parser("list", "list all available plugins")
        .addDefault[(ParsingResult, Configuration) => Unit](
          "func",
          listPlugins,
          "list all installed plugins"))
    .addSubparser(
      Parser("available", "list all available plugins")
        .addFlag("all", "a", "all", "if set list all available versions")
        .addOptional("filter",
                     "f",
                     "filter",
                     None,
                     "a regexp filter for displayed plugins")
        .addDefault[(ParsingResult, Configuration) => Unit](
          "func",
          availablePlugins,
          "list all available plugins"))
    .addSubparser(Parser("install", "install a plugin")
      .addFlag("update",
               "u",
               "update",
               "if set updates the plugin to the most current version")
      .addFlag("force",
               "f",
               "force",
               "if set removes an already installed version of the plugin")
      .addOptional(
        "version",
        "r",
        "revision",
        None,
        "if set will install (if available) the specified version of the plugin")
      .addPositional("plugin", "the name of the plugin")
      .addDefault[(ParsingResult, Configuration) => Unit](
        "func",
        installPlugin,
        "install/update a named plugin"))

  /** main to list all plugins already installed
    *
    * @param pargs the command line arguments
    * @param conf the parsed configuration
    */
  private def listPlugins(@nowarn pargs: ParsingResult,
                          conf: Configuration): Unit = {
    val manager = getPluginManager(conf.plugin)
    println("Installed Plugins:")
    manager.getInstalledPlugins.foreach {
      case (name, version) => println(s"* $name $version")
    }
  }

  /** utility function to get all releases of a specified remote plugin
    *
    * @param client the http client to use
    * @param remote the remote plugin repository of interest
    * @return the download details
    */
  private def getReleases(
      client: HttpClient,
      remote: RemotePluginConfig): List[RemoteGithubRelease] = {
    val request = HttpRequest.newBuilder(URI.create(
      s"https://api.github.com/repos/${remote.owner}/${remote.repo}/releases"))
    val json = client.send(request.build(), BodyHandlers.ofString()).body()
    val plugins = JsonParser(json)
      .asInstanceOf[JsArray]
      .elements
      .map(elem => RemoteGithubRelease(elem.asJsObject))
      .sorted
      .reverse
    plugins.toList
  }

  /** list all available plugins based on the configured remote repositories
    *
    * @param pargs the parsed command line arguments
    * @param conf the parsed configuration
    */
  private def availablePlugins(pargs: ParsingResult,
                               conf: Configuration): Unit = {
    val client = HttpClient.newHttpClient()
    println("Available Plugins:")
    val filter = pargs.get[OptionalValue[String]]("filter").value match {
      case Some(value) => value.r.unanchored //either restrict to the filter
      case None        => ".*".r.unanchored // or if no filter list everything
    }
    conf.plugin.available.filter(plugin => filter.matches(plugin._1)).foreach {
      case (name, remote) =>
        val versions = getReleases(client, remote)
        if (pargs.getValue[Boolean]("all")) {
          versions.foreach { version =>
            println(s"+ $name  ${version.getVersion}")
          }
        } else {
          println(s"+ $name ${versions.head.getVersion}")
        }
    }
  }

  /** main to install a named plugin
    *
    * @param pargs the parsed command line arguments
    * @param conf the parsed configuration
    */
  private def installPlugin(pargs: ParsingResult, conf: Configuration): Unit = {
    val client = HttpClient
      .newBuilder()
      .followRedirects(HttpClient.Redirect.ALWAYS)
      .build()
    val forced = pargs.getValue[Boolean]("force")
    val update = pargs.getValue[Boolean]("update")
    val version = pargs.get[OptionalValue[String]]("version").value
    if (update)
      assert(
        version.isEmpty,
        "you cannot update to a specific version use install --force --release <version> instead")
    val plugin = pargs.getValue[String]("plugin")
    conf.plugin.available.find {
      case (key, _) => plugin.r.unanchored.matches(key)
    } match {
      case Some(remoteTarget) =>
        val releases = getReleases(client, remoteTarget._2)
        val target = version match {
          case Some(version) =>
            releases
              .find(_.getVersion == version)
              .getOrElse(throw NoSuchPlugin(plugin, Some(version)))
          case None => releases.head
        }
        info(s"installing plugin ${remoteTarget._1} ${target.getVersion}")
        val manager = getPluginManager(conf.plugin)
        manager.getInstalledPlugins.find {
          case (name, _) => name == remoteTarget._1
        } match {
          case Some((_, version)) =>
            warn(s"a previous version ($version) is already installed")
            if (forced || update) {
              warn("removing old version")
              manager.removePlugin(remoteTarget._1)
            } else {
              return
            }
          case None =>
        }
        info(s"downloading plugin ${remoteTarget._1} ${target.getVersion}")
        val request =
          HttpRequest.newBuilder().uri(URI.create(target.jarDownloadLink))
        val ret = client.send(request.build(), BodyHandlers.ofByteArray())
        if (ret.statusCode() != 200) {
          throw new RuntimeException(
            s"downloading jar returned ${ret.statusCode()} // ${target.jarDownloadLink}")
        }
        val jarName = target.jarDownloadLink.split("/").last
        val fileWriter = new FileOutputStream(
          new File(conf.plugin.folder + "/" + jarName))
        try {
          fileWriter.write(ret.body())
        } finally {
          fileWriter.close()
        }
      case None => throw NoSuchPlugin(plugin, version)
    }
  }

}
