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
class TrafficCollectionCache(var trafficCollections: List[TrafficCollection])
    extends LogSupport {

  /** mapping of analysis id to traffic collection
    */
  private def analysisMap: Map[Int, List[TrafficCollection]] =
    trafficCollections.groupBy(_.getAnalysis)

  /** mapping of analysis,interfaces to traffic collections
    */
  private def analysisInterfaceMap: Map[(Int, Int), List[TrafficCollection]] = {
    trafficCollections.filter(_.getInterface.nonEmpty).groupBy { elem =>
      (elem.getAnalysis, elem.getInterface.get)
    }
  }

  def get: List[TrafficCollection] = trafficCollections

  def get(analysis: Int): List[TrafficCollection] =
    analysisMap.getOrElse(analysis, List())

  def get(analysis: Int, interface: Int): List[TrafficCollection] =
    analysisInterfaceMap.getOrElse((analysis, interface), List())

  def delete(analysis: Int): Unit =
    trafficCollections = trafficCollections.filterNot(_.getAnalysis == analysis)
}

/** companion object
  *
  * @author Simon Koch
  */
object TrafficCollectionCache extends LogSupport {

  /** the map of experiment id to cache entry
    */
  private val cache: MMap[Int, TrafficCollectionCache] = MMap()
  // Safe the current chunk offset
  private val cacheChunks: MMap[Int, Int] = MMap()
  /** get an cache entry for a given experiment
    *
    * @param experiment the experiment id
    * @param chunks the number of app analysis to load in the cache
    * @param database the database connection
    * @return the cach entry
    */
  private def getCacheEntry(experiment: Int, chunks: Int = -1)(
      implicit database: Database): TrafficCollectionCache = {
    cache.get(experiment) match {
      case Some(value) =>
        value
      case None =>
        val entry: TrafficCollectionCache =
          new TrafficCollectionCache(
            TrafficCollection.getTrafficCollections(experiment, chunks, 0))
        cacheChunks.put(experiment, chunks)
        cache.addOne(experiment -> entry)
        entry
    }
  }

  /** Add apps to the cache for the experiment id
    *
    * @param experiment the experiment id
    * @param database the database connection
    * @return the cach entry
    */
  private def getNewCacheEntry(experiment: Int, chunks: Int)(
      implicit database: Database): TrafficCollectionCache = {
    cache.get(experiment) match {
      case Some(value) =>
        val newTraffic = TrafficCollection.getTrafficCollections(experiment, chunks, cacheChunks.getOrElse(experiment, 0))
        val oldTraffic = value.get

        // If there are no new added entries
        if (newTraffic.length == 0) {
          return new TrafficCollectionCache(List())
        }

        val entry: TrafficCollectionCache =
          new TrafficCollectionCache(newTraffic ++ oldTraffic)
        cache.put(experiment, entry)
        cacheChunks.put(experiment, cacheChunks.getOrElse(experiment, 0) + chunks)
        entry
      case None =>
        val entry: TrafficCollectionCache =
          new TrafficCollectionCache(
            TrafficCollection.getTrafficCollections(experiment, chunks, 0))
        cacheChunks.put(experiment, chunks)
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

  /** get traffic collection for a experiment and load it into chunks
    *
    * @param experiment the experiment id
    * @param analysis the analysis id
    * @param chunks the number of app analysis to load in the cache
    * @param database the database connection
    * @return the list of traffic collections
    */
  def getTrafficCollectionsChunks(experiment: Int, analysis: Int, chunks: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    var trafficCollection: List[TrafficCollection] = getCacheEntry(experiment, chunks).get(analysis)

    // Check if the analysis run is contained in the cache
    while (trafficCollection.isEmpty) {
      val newCache = getNewCacheEntry(experiment, chunks)
      if (newCache.get.isEmpty) {
        // No new app added in the cache, it just doesn't exist
        return List()
      } else {
        trafficCollection = newCache.get(analysis)
      }
    }

    // Delete entry out of cache
    cache.get(experiment).getOrElse(new TrafficCollectionCache(List())).delete(analysis)
    return trafficCollection
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
