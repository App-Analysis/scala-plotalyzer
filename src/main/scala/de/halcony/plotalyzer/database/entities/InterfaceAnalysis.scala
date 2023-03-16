package de.halcony.plotalyzer.database.entities

import de.halcony.plotalyzer.database.Database
import de.halcony.plotalyzer.database.entities.trafficcollection.{
  TrafficCollection,
  TrafficCollectionCache
}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.JsValue
import wvlet.log.LogSupport

import java.io.File
import java.time.ZonedDateTime

/** an interface analysis
  *
  * @author Simon Koch
  *
  * @param id the id of the interface analysis
  * @param experiment the corresponding experiment id
  * @param app the corresponding mobile app
  * @param description the stored description for the analysis
  * @param start the start time of the analysis
  * @param end the end time of the analysis
  * @param success whether or not the analysis was a success
  * @param database the database connection
  */
class InterfaceAnalysis(id: Int,
                        experiment: Int,
                        app: MobileApp,
                        description: String,
                        start: ZonedDateTime,
                        end: ZonedDateTime,
                        success: Boolean)(implicit database: Database)
    extends LogSupport {

  /** dump all screenshots of the interface analysis as a chain starting in baseFolder
    *
    * @param baseFolder the folder where to start dumping the screenshots
    */
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

  /** the preferences of the app analyzed
    */
  private var appPreferences: Option[List[AppPreferences]] = None

  /** get all app preferences
    *
    * @return the list of app preferences in order of interactions
    */
  def getAppPreferences: List[AppPreferences] = synchronized {
    appPreferences match {
      case Some(value) => value
      case None =>
        appPreferences = Some(AppPreferences.get(this))
        appPreferences.get
    }
  }

  /** the traffic collection during the interface analysis in order of interactions
    */
  private var trafficCollections: Option[List[TrafficCollection]] = None

  /** get the traffic collection during app analysis in order of interaction
    *
    * @return the list of traffic collections
    */
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

  /** teh interface chain associated with the interface analysis
    */
  private var interfaceChain: Option[InterfaceInteractionChain] = None

  /** get the interface chain associated with the interface analysis
    *
    * @return the interface interaction chain of the interface analysis
    */
  def getInterfaceChain: InterfaceInteractionChain = synchronized {
    interfaceChain match {
      case Some(value) => value
      case None =>
        interfaceChain = Some(InterfaceInteractionChain.get(this))
        interfaceChain.get
    }
  }

  /** errors that occurred during the interface analysis
    */
  private var errors: Option[List[InterfaceAnalysisError]] = None

  /** get the errors that occurred during the interface analysis
    *
    * @return the errors that occured during the interface analysis
    */
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

/** companion object
  *
  * @author Simon Koch
  *
  */
object InterfaceAnalysis {

  /** convert a selected entity into an interface analysis object
    *
    * expects the columns id,experiment, app..., description, start_time, end_time, success
    *
    * @param entity the entity to convert
    * @param database the database connection
    * @return
    */
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

  /** get all interface analysis associated with an experiment
    *
    * @param experiment the experiment of interest
    * @param database the database connection
    * @return the list of corresponding interface analysis
    */
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

  /** get only the interface analysis of the experiment for a specific app
    *
    * @param experiment the experiment of interest
    * @param app the app of interest
    * @param database the database connection
    * @return the list of fitting interface analysis
    */
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
