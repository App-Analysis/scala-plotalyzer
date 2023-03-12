package de.tubs.cs.ias.plotalyzer.plugins

import org.clapper.classutil.{ClassFinder, ClassInfo}
import wvlet.log.LogSupport
import java.io.File
import java.net.{URL, URLClassLoader}

/** The PluginManager responsible for finding and loading available plugins
  *
  * @author Simon Koch
  */
object PluginManager extends LogSupport {

  /** the directory in which to store the plugins
    * todo: move to config file
    */
  val PLUGIN_DIRECTOR = "./plugins/"

  /** the fully qualified name of the AnalysisPlugin trait
    */
  val PLUGIN_INTERFACE = "de.tubs.cs.ias.plotalyzer.plugins.AnalysisPlugin"

  /** the jars contained in the PLUGIN_DIRECTORY folder on startup
    */
  val jars: List[File] = new File(PLUGIN_DIRECTOR).listFiles
    .filter(_.getPath.endsWith(".jar"))
    .toList

  /** a map of the plugins and the name of the main classes
    *
    */
  private val pluginMap: Map[String, (ClassInfo, File)] = {
    jars.flatMap { jar =>
      val finder = ClassFinder(List(jar))
      println(finder.getClasses().map(_.interfaces).toList)
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
  def getPlugins: Set[String] = pluginMap.keySet

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
      de.tubs.cs.ias.plotalyzer.Plotalyzer.getClass.getClassLoader
    val childClassLoader =
      URLClassLoader.newInstance(urls.toArray, parentClassLoader)
    try {
      info(s"loading plugin ${classInfo._1.name}")
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
