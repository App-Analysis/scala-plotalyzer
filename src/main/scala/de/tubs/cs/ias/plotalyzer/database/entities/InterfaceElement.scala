package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

/** an element of an interface
  *
  * @author Simon Koch
  * todo: this could be a case class as it is not mutable
  *
  * @param id the id of the interface
  * @param belongsTo the id of the interface this element belongs to
  * @param text the text contained in the interface element
  * @param clickable whether or not the element is clickable
  * @param database the database connection
  */
class InterfaceElement(id: Int,
                       belongsTo: Int,
                       text: String,
                       clickable: Boolean)(implicit database: Database) {

  def getId: Int = id
  def getBelongsTo: Int = belongsTo
  def getText: String = text.toLowerCase.trim

  def getOriginalText: String = text

  def getClickable: Boolean = clickable

  /** retrieve the screenshot of the element from the database
    *
    * @return the screenshot
    */
  def getScreenshot: Option[BufferedImage] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT screenshot FROM interfaceelement WHERE id = $id AND screenshot IS NOT NULL"""
        .map { entity =>
          InterfaceElement.getBufferedImage(entity.bytes("screenshot"))
        }
        .first
        .apply()
    }
  }

  /** check whether this element has a screenshot on file
    *
    * @return true if a screenshot exists else false
    */
  def hasScreenshot: Boolean = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT TRUE AS succ FROM interfaceelement WHERE id = $id AND screenshot IS NOT NULL"""
        .map(_.boolean("succ"))
        .first
        .apply()
        .getOrElse(false)
    }
  }

}

/** companion object
  *
  * @author Simon Koch
  *
  */
object InterfaceElement {

  /** create an interface element from an retrieved database entity
    *
    * expects the columns id, belongs_to, text, clickable
    *
    * @param entity the entity
    * @param database the database connection
    * @return the extracted interface element
    */
  def apply(entity: WrappedResultSet)(
      implicit database: Database): InterfaceElement = {
    new InterfaceElement(
      entity.int("id"),
      entity.int("belongs_to"),
      entity.string("text"),
      entity.boolean("clickable")
    )
  }

  /** get the elements of an interface
    *
    * @param interface the id of the interface
    * @param database the database connection
    * @return the list of interface elements
    */
  def get(interface: Int)(
      implicit database: Database): List[InterfaceElement] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                       belongs_to,
                       text,
                       clickable
                 FROM interfaceelement
                 WHERE belongs_to = $interface
             """.map(InterfaceElement.apply).toList.apply()
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
