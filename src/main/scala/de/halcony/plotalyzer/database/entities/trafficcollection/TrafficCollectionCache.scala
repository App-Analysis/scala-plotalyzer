package de.halcony.plotalyzer.database.entities.trafficcollection

import de.halcony.plotalyzer.database.Database
import wvlet.log.LogSupport

import scala.collection.mutable.{Map => MMap}

/** a cache class to structure retrieval of large traffic collection request sets more efficiently
  *
  * @author Simon Koch
  *
  * @param trafficCollections the list of traffic collections of interest
  */
class TrafficCollectionCache(trafficCollections: List[TrafficCollection])
    extends LogSupport {

  /** mapping of analysis id to traffic collection
    */
  private val analysisMap: Map[Int, List[TrafficCollection]] =
    trafficCollections.groupBy(_.getAnalysis)

  /** mapping of analysis,interfaces to traffic collections
    */
  private val analysisInterfaceMap: Map[(Int, Int), List[TrafficCollection]] = {
    trafficCollections.filter(_.getInterface.nonEmpty).groupBy { elem =>
      (elem.getAnalysis, elem.getInterface.get)
    }
  }

  def get: List[TrafficCollection] = trafficCollections

  def get(analysis: Int): List[TrafficCollection] =
    analysisMap.getOrElse(analysis, List())

  def get(analysis: Int, interface: Int): List[TrafficCollection] =
    analysisInterfaceMap.getOrElse((analysis, interface), List())

}

/** companion object
  *
  * @author Simon Koch
  */
object TrafficCollectionCache extends LogSupport {

  /** the map of experiment id to cache entry
    */
  private val cache: MMap[Int, TrafficCollectionCache] = MMap()

  /** get an cache entry for a given experiment
    *
    * @param experiment the experiment id
    * @param database the database connection
    * @return the cach entry
    */
  private def getCacheEntry(experiment: Int)(
      implicit database: Database): TrafficCollectionCache = {
    cache.get(experiment) match {
      case Some(value) =>
        value
      case None =>
        val entry: TrafficCollectionCache =
          new TrafficCollectionCache(
            TrafficCollection.getTrafficCollections(experiment))
        cache.addOne(experiment -> entry)
        entry
    }
  }

  /** get traffic collection for experiment
    *
    * @param experiment the experiment id
    * @param database the database connection
    * @return the list of traffic collections
    */
  def getTrafficCollections(experiment: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get
  }

  /** get traffic collection for a experiment,analysis pair
    *
    * @param experiment the experiment id
    * @param analysis the analysis id
    * @param database the database connection
    * @return the list of traffic collections
    */
  def getTrafficCollections(experiment: Int, analysis: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get(analysis)
  }

  /** get the traffic collection for a experiment,analysis,interface triplet
    *
    * @param experiment the experiment id
    * @param analysis the analysis id
    * @param interface the interface id
    * @param database the database connection
    * @return the list of traffic collections (should be only a single one)
    */
  def getTrafficCollections(experiment: Int, analysis: Int, interface: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get(analysis, interface)
  }

}
