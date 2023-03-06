package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import wvlet.log.LogSupport

import java.io.File

sealed trait InterfaceInteractionChainElement {

  val interface: Interface

  def getLastInterface: Interface

  def dumpScreenshots(folder: String): Option[String]

}
sealed case class ChainEnd(override val interface: Interface)
    extends InterfaceInteractionChainElement {

  override def dumpScreenshots(folder: String): Option[String] = {
    interface.dumpScreenshots(folder)
    None
  }

  override def getLastInterface: Interface = interface

}

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

object InterfaceInteractionChain {

  def get(analysis: InterfaceAnalysis)(
      implicit database: Database): InterfaceInteractionChain = {
    new InterfaceInteractionChain(Interface.getAll(analysis))
  }

}
