package de.tubs.cs.ias.plotalyzer.utility

import de.tubs.cs.ias.plotalyzer.database.entities.InterfaceElement
import de.tubs.cs.ias.plotalyzer.utility.color.ColorRepresentation

object CompareElements {

  /** calculation if any element is larger
    *
    * @param highlighted the elements we want to check if they are larger
    * @param against the elements we want to check if they are larger against
    * @param maxFactor the maximum size difference factor after which we consider an element larger
    * @return whether or not an element existed passing the provided threshold
    */
  def elementSizeHighlighted(highlighted: List[InterfaceElement],
                             against: List[InterfaceElement],
                             maxFactor: Double): Boolean = {
    val affirmativeSize = highlighted
      .map(elem =>
        elem.getScreenshot.get.getWidth * elem.getScreenshot.get.getHeight)
      .map(_.toDouble)
    val negativeSize = against
      .map(elem =>
        elem.getScreenshot.get.getWidth * elem.getScreenshot.get.getHeight)
      .map(_.toDouble)
    // does a negative button exists for which a positive button exists that is not maxFactor larger
    !negativeSize.exists(neg =>
      affirmativeSize.exists(afirm => afirm / neg < maxFactor))
  }

  /** uses CIDE2000 DeltaE calculation to check if any element is highlighted
    *
    * @param highlighted the elements we want to check if they are highlighted
    * @param against the elements we want to check if they are highlighted against
    * @param maxDelta the maximum delta after which we consider it highlighted
    * @return whether or not an element existed passing the provided threshold
    */
  def elementColorHighlighted(highlighted: List[InterfaceElement],
                              against: List[InterfaceElement],
                              maxDelta: Double): Boolean = {
    val affirmativeColors = highlighted.map(elem =>
      ColorRepresentation.colorOfImage(elem.getScreenshot.get))
    val negativeColors = against.map(elem =>
      ColorRepresentation.colorOfImage(elem.getScreenshot.get))
    // does a negative button exists for which a positive button exists that is not highlighted
    !negativeColors.exists { neg =>
      affirmativeColors.exists { afirm =>
        ColorRepresentation.deltaE(neg, afirm) < maxDelta
      }
    }
  }

}
