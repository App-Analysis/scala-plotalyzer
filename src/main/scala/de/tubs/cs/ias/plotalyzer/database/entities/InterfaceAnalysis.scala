package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.{
  TrafficCollection,
  TrafficCollectionCache
}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.JsValue
import wvlet.log.LogSupport

import java.io.File
import java.time.ZonedDateTime

class InterfaceAnalysis(id: Int,
                        experiment: Int,
                        app: MobileApp,
                        description: String,
                        start: ZonedDateTime,
                        end: ZonedDateTime,
                        success: Boolean)(implicit database: Database)
    extends LogSupport {

  def dumpScreenshot(baseFolder: String): Unit = {
    val folder = new File(baseFolder + "/" + getDescription.replace(' ', '_'))
    assert(!folder.exists())
    folder.mkdirs()
    getInterfaceChain.dumpScreenshots(folder.getPath + "/")
  }

  def getId: Int = id
  def getExperimentId: Int = experiment
  def getApp: MobileApp = app
  def getDescription: String = description
  def getStart: ZonedDateTime = start
  def getEnd: ZonedDateTime = end
  def getSuccess: Boolean = success

  private var appPreferences: Option[List[AppPreferences]] = None
  def getAppPreferences: List[AppPreferences] = synchronized {
    appPreferences match {
      case Some(value) => value
      case None =>
        appPreferences = Some(AppPreferences.get(this))
        appPreferences.get
    }
  }

  private var trafficCollections: Option[List[TrafficCollection]] = None
  def getTrafficCollection: List[TrafficCollection] = synchronized {
    trafficCollections match {
      case Some(value) => value
      case None =>
        trafficCollections = Some(
          TrafficCollectionCache.getTrafficCollections(this.getExperimentId,
                                                       this.getId))
        trafficCollections.get
    }
  }

  private var interfaceChain: Option[InterfaceInteractionChain] = None
  def getInterfaceChain: InterfaceInteractionChain = synchronized {
    interfaceChain match {
      case Some(value) => value
      case None =>
        interfaceChain = Some(InterfaceInteractionChain.get(this))
        interfaceChain.get
    }
  }

  private var errors: Option[List[InterfaceAnalysisError]] = None
  def getErrors: List[InterfaceAnalysisError] = synchronized {
    errors match {
      case Some(value) => value
      case None =>
        errors =
          Some(InterfaceAnalysisError.getInterfaceAnalysisErrors(List(getId)))
        errors.get
    }
  }

  def toJson: JsValue = throw new NotImplementedError()

}

object InterfaceAnalysis {

  def apply(entity: WrappedResultSet)(
      implicit database: Database): InterfaceAnalysis = {
    new InterfaceAnalysis(
      entity.int("id"),
      entity.int("experiment"),
      MobileApp(entity),
      entity.string("description"),
      entity.zonedDateTime("start_time"),
      entity.zonedDateTime("end_time"),
      entity.boolean("success")
    )
  }

  def get(experiment: Experiment)(
      implicit database: Database): List[InterfaceAnalysis] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     experiment,
                     app_id,
                     app_version,
                     app_os,
                     description,
                     start_time,
                     end_time,
                     success
              FROM interfaceanalysis
              WHERE experiment = ${experiment.getId}
           """.map(InterfaceAnalysis.apply).toList.apply()
    }
  }

  def get(experiment: Experiment, app: String)(
      implicit database: Database): List[InterfaceAnalysis] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     experiment,
                     app_id,
                     app_version,
                     app_os,
                     description,
                     start_time,
                     end_time,
                     success
              FROM interfaceanalysis
              WHERE experiment = ${experiment.getId} AND
                    app_id = $app
           """.map(InterfaceAnalysis.apply).toList.apply()
    }
  }

}
