package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import wvlet.log.LogSupport
import java.io.File

/** trait representing an element in an interaction chain
  *
  * @author Simon Koch
  *
  */
sealed trait InterfaceInteractionChainElement {

  /** the interface of the current chain step
    */
  val interface: Interface

  /** get the last interface in the chain
    *
    * @return the last interface in the chain
    */
  def getLastInterface: Interface

  /** write the screenshots of the current chain step into folder
    *
    * @param folder the folder into which to write the screenshots
    * @return the current folder name of the chain step
    */
  def dumpScreenshots(folder: String): Option[String]

}

/** the last element in an interface interaction chain
  *
  * @author Simon Koch
  *
  * @param interface the interface belonging to this step
  */
sealed case class ChainEnd(override val interface: Interface)
    extends InterfaceInteractionChainElement {

  override def dumpScreenshots(folder: String): Option[String] = {
    interface.dumpScreenshots(folder)
    None
  }

  override def getLastInterface: Interface = interface

}

/** an interaction step that unexpectedly stops an interaction
  *
  * @author Simon Koch
  *
  * @param interface the interface belonging to this step
  * @param action the action performed in this step
  */
sealed case class StoppingInteraction(override val interface: Interface,
                                      action: InterfaceElementInteraction)
    extends InterfaceInteractionChainElement {

  override def getLastInterface: Interface = interface

  override def dumpScreenshots(folder: String): Option[String] = {
    interface.dumpScreenshots(folder)
    val next = folder + "/" + action.getOnElement.toString
    new File(next).mkdir()
    Some(next)
  }

}

/** an interaction step that leads to a new interface
  *
  * @author Simon Koch
  *
  * @param interface the interface belonging to this step
  * @param action the action performed in this step
  * @param leadingTo the next chain element following this action
  */
sealed case class NonStoppingInteraction(
    override val interface: Interface,
    action: InterfaceElementInteraction,
    leadingTo: InterfaceInteractionChainElement)
    extends InterfaceInteractionChainElement {

  override def dumpScreenshots(folder: String): Option[String] = {
    interface.dumpScreenshots(folder)
    val next = folder + "/" + action.getOnElement.toString + "/"
    new File(next).mkdir()
    Some(next)
  }

  override def getLastInterface: Interface = leadingTo.getLastInterface

}

/** class representing the overall interface interaction chain
  *
  * @author Simon Koch
  * todo: this could be made a case class as it is not mutable
  *
  * @param start the list of interfaces to form the chain from
  * @param database the database connection
  */
class InterfaceInteractionChain(start: List[Interface])(
    implicit database: Database)
    extends LogSupport {

  def dumpScreenshots(baseFolder: String): Unit = {
    chain match {
      case Some(chain) =>
        var currentChainElement: Option[InterfaceInteractionChainElement] =
          Some(chain)
        var currentFolder = baseFolder
        while (currentChainElement.nonEmpty) {
          currentChainElement.get match {
            case x: ChainEnd =>
              x.dumpScreenshots(currentFolder)
              currentChainElement = None
            case x: StoppingInteraction =>
              x.dumpScreenshots(currentFolder)
              currentChainElement = None
            case x: NonStoppingInteraction =>
              currentFolder = x.dumpScreenshots(currentFolder).get
              currentChainElement = Some(x.leadingTo)
          }
        }
      case None => warn("there is no chain")
    }
  }

  /** the chain formed out of the provided interfaces
    */
  val chain: Option[InterfaceInteractionChainElement] = {
    val interfaceMap: Map[Int, Interface] =
      start.groupBy(_.getId).map(elem => elem._1 -> elem._2.head)
    def construct(interface: Interface): InterfaceInteractionChainElement = {
      val next: Option[InterfaceElementInteraction] =
        InterfaceElementInteraction.get(interface)
      next match {
        case Some(action) =>
          action.getLeadingTo match {
            case Some(interfaceId) =>
              NonStoppingInteraction(
                interface,
                action,
                construct(interfaceMap.getOrElse(
                  interfaceId,
                  throw new RuntimeException(
                    s"follow chain interface with id $interfaceId does not exist")))
              )
            case None =>
              StoppingInteraction(interface, action)
          }
        case None => ChainEnd(interface)
      }
    }
    if (interfaceMap.keySet.nonEmpty)
      Some(construct(interfaceMap(interfaceMap.keySet.min)))
    else
      None
  }

}

/** companion object
  *
  * @author Simon Koch
  *
  */
object InterfaceInteractionChain {

  /** create an interface interaction chain of a given analysis
    *
    * @param analysis the analysis to extract the interaction chain from
    * @param database the database connection
    * @return the interface interaction chain
    */
  def get(analysis: InterfaceAnalysis)(
      implicit database: Database): InterfaceInteractionChain = {
    new InterfaceInteractionChain(Interface.getAll(analysis))
  }

}
