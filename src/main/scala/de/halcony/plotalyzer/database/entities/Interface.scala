package de.halcony.plotalyzer.database.entities

import de.halcony.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import wvlet.log.LogSupport
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, File}
import javax.imageio.ImageIO

/** an interface of an app
  *
  * @author Simon Koch
  *
  * @param id the id of the interface
  * @param analysis the id of the corresponding analysis
  * @param comment the corresponding comment stored in the database
  * @param database the database connection
  */
class Interface(id: Int, analysis: Int, comment: String)(
    implicit database: Database)
    extends LogSupport {

  def getId: Int = id
  def getAnalysisId: Int = analysis
  def getComment: String = comment

  /** write the screenshot of the interface into folder
    *
    * @param folder the folder into which to store the screenshot
    */
  def dumpScreenshots(folder: String): Unit = {
    this.getScreenshot match {
      case Some(value) =>
        val mainScreenshot = new File(folder + "main.png")
        ImageIO.write(value, "png", mainScreenshot)
        getInterfaceElements.foreach { elem =>
          elem.getScreenshot match {
            case Some(screenshot) =>
              val elementScreenshot =
                new File(folder + elem.getId.toString + ".png")
              ImageIO.write(screenshot, "png", elementScreenshot)
            case None =>
              warn("interface element is missing screenshot")
          }
        }
      case None =>
        warn("interface is missing screenshot")
    }
  }

  /** the interface elements of the interface
    */
  private val interfaceElements: List[InterfaceElement] =
    InterfaceElement.get(this.id)

  /** get the interface elements
    *
    * @return the interface elements of the interface
    */
  def getInterfaceElements: List[InterfaceElement] = interfaceElements

  /** whether or not the interface has any elements
    *
    * @return true if set of elements is non empty else false
    */
  def hasElements: Boolean = getInterfaceElements.nonEmpty

  /** get the amount of elements of the interface
    *
    * @return the amount of elements of the interface
    */
  def elementCount: Int = getInterfaceElements.length

  /** retrieve the screenshot stored for the interface
    *
    * @return the screenshot of the interface
    */
  def getScreenshot: Option[BufferedImage] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT screenshot FROM interface WHERE id = $id AND screenshot IS NOT NULL"""
        .map { entity =>
          Interface.getBufferedImage(entity.bytes("screenshot"))
        }
        .first
        .apply()
    }
  }

  /** whether or not the interface has a screenshot
    *
    * @return true if a screenshot is stored else false
    */
  def hasScreenshot: Boolean = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT TRUE AS succ FROM interface WHERE id = $id AND screenshot IS NOT NULL"""
        .map(_.boolean("succ"))
        .first
        .apply()
        .getOrElse(false)
    }
  }

  /** whether or not the interface and all elements have screenshots associated
    *
    * @return true if it is the case else false
    */
  def isFullyScreenshotted: Boolean = {
    hasScreenshot && interfaceElements.forall(_.hasScreenshot)
  }

}

/** companion object
  *
  */
object Interface {

  /** convert a database entity into a interface
    *
    * expects the columns id,analysis,comment
    *
    * @param entity the database entry
    * @param database the database connection
    * @return
    */
  def apply(entity: WrappedResultSet)(
      implicit database: Database): Interface = {
    new Interface(
      entity.int("id"),
      entity.int("analysis"),
      entity.string("comment")
    )
  }

  /** get all interfaces of a given interface analysis
    *
    * @param analysis the interface analysis of interest
    * @param database the database connection
    * @return the list of corresponding interfaces
    */
  def getAll(analysis: InterfaceAnalysis)(
      implicit database: Database): List[Interface] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     analysis,
                     comment
              FROM interface
              WHERE analysis = ${analysis.getId}
           """.map(Interface.apply).toList.apply()
    }
  }

  /** get the first interface associated with an interface analysis
    *
    * @param analysis the analysis of interest
    * @param database the database connection
    * @return the first interface
    */
  def getStart(analysis: InterfaceAnalysis)(
      implicit database: Database): Interface = {
    database.withDatabaseSession { implicit session =>
      sql""" SELECT id,
                      analysis,
                      comment
               FROM interface
               WHERE id = (SELECT min(id)
                           FROM interface
                           WHERE analysis = ${analysis.getId})
           """
        .map(Interface.apply)
        .first
        .apply()
        .getOrElse(throw new RuntimeException(
          s"interface analysis ${analysis.getId} has no interface"))
    }
  }

  /** get an interface by id
    *
    * @param id th id of the interface
    * @param database the database connection
    * @return the interface
    */
  def getById(id: Int)(implicit database: Database): Interface = {
    database.withDatabaseSession { implicit session =>
      sql""" SELECT id,
                      analysis,
                      comment
               FROM interface
               WHERE id = $id
           """
        .map(Interface.apply)
        .first
        .apply()
        .getOrElse(throw new RuntimeException(s"interface $id does not exist"))
    }
  }

  /** helper function to create an image from the stored bytes
    *
    * @param bytes the stored bytes
    * @return the corresponding image
    */
  protected def getBufferedImage(bytes: Array[Byte]): BufferedImage = {
    val inputStream = new ByteArrayInputStream(bytes)
    ImageIO.read(inputStream)
  }

}
