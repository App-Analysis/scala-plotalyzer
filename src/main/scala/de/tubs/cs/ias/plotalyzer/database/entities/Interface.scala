package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.utility.Picturesque
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import wvlet.log.LogSupport

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

class Interface(id: Int, analysis: Int, comment: String)(
    implicit database: Database)
    extends LogSupport {

  def getId: Int = id
  def getAnalysisId: Int = analysis
  def getComment: String = comment

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

  private val interfaceElements: List[InterfaceElement] =
    InterfaceElement.get(this.id)

  def getInterfaceElements: List[InterfaceElement] = interfaceElements

  def hasElements: Boolean = getInterfaceElements.nonEmpty

  def elementCount: Int = getInterfaceElements.length

  def getScreenshot: Option[BufferedImage] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT screenshot FROM interface WHERE id = $id AND screenshot IS NOT NULL"""
        .map { entity =>
          Picturesque.getBufferedImage(entity.bytes("screenshot"))
        }
        .first
        .apply()
    }
  }

  def hasScreenshot: Boolean = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT TRUE AS succ FROM interface WHERE id = $id AND screenshot IS NOT NULL"""
        .map(_.boolean("succ"))
        .first
        .apply()
        .getOrElse(false)
    }
  }

  def isFullyScreenshotted: Boolean = {
    hasScreenshot && interfaceElements.forall(_.hasScreenshot)
  }

}

object Interface {

  def apply(entity: WrappedResultSet)(
      implicit database: Database): Interface = {
    new Interface(
      entity.int("id"),
      entity.int("analysis"),
      entity.string("comment")
    )
  }

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

}
