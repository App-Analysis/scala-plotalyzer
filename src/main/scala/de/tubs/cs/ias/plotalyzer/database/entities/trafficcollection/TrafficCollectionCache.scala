package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import wvlet.log.LogSupport

import scala.collection.mutable.{Map => MMap}

class TrafficCollectionCache(trafficCollections: List[TrafficCollection])
    extends LogSupport {

  private val analysisMap: Map[Int, List[TrafficCollection]] =
    trafficCollections.groupBy(_.getAnalysis)
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

object TrafficCollectionCache extends LogSupport {

  private val cache: MMap[Int, TrafficCollectionCache] = MMap()

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

  def getTrafficCollections(experiment: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get
  }

  def getTrafficCollections(experiment: Int, analysis: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get(analysis)
  }

  def getTrafficCollections(experiment: Int, analysis: Int, interface: Int)(
      implicit database: Database): List[TrafficCollection] = synchronized {
    getCacheEntry(experiment).get(analysis, interface)
  }

}
