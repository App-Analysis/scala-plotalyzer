package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import de.tubs.cs.ias.plotalyzer.database.entities._
import de.tubs.cs.ias.plotalyzer.utility.CompareElements
import spray.json.{JsArray, JsBoolean, JsNull, JsObject, JsString, JsValue}
import scala.collection.mutable.ListBuffer

class AppConsentDialogAnalysis(interfaceAnalysis: List[InterfaceAnalysis],
                               conf: ConsentDialogAnalysisConfig) {

  def getInterfaceAnalysis: List[InterfaceAnalysis] = interfaceAnalysis

  private val analysisIssues: ListBuffer[String] = ListBuffer()
  private def addIssue(string: String): Unit = {
    if (string != ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR) {
      analysisIssues.addOne(string)
    }
  }

  def getAnalysisIds: List[Int] = interfaceAnalysis.map(_.getId)

  private def hasBreakingIssue: Boolean = {
    val ignore = Set(
      ConsentDialogAnalysisIssues.NONE_WITH_ACCEPT_REJECT_ANALYSIS)
    (analysisIssues.toSet -- ignore).nonEmpty
  }

  def getIssues: List[String] = analysisIssues.toList

  private val app: Option[MobileApp] =
    interfaceAnalysis.map(_.getApp).distinct match {
      case Nil =>
        // this is an issue indicating something larger
        throw new RuntimeException("there is no app")
      case single :: Nil => Some(single)
      case _ =>
        addIssue(ConsentDialogAnalysisIssues.MULTIPLE_APPS)
        None
    }

  def getApp: MobileApp = app.get

  private var initialDialog: Option[InterfaceAnalysis] = None
  private var initialTraffic: Option[InterfaceAnalysis] = None
  private var accept: Option[InterfaceAnalysis] = None
  private var reject: Option[InterfaceAnalysis] = None
  private object ConsentDialogTypes extends Enumeration {
    type ConsentDialogType = Value
    // the delta between MAYBE and regular is only for internal book keeping
    val LINK, NOTICE, MAYBE_NOTICE, DIALOG, MAYBE_DIALOG, NONE = Value
  }
  import ConsentDialogTypes._
  private var consentDialogType: Option[ConsentDialogType] = None
  def getConsentDialogType: String = consentDialogType match {
    case Some(value) =>
      value match {
        case LINK   => "Link"
        case NOTICE => "Notice"
        case DIALOG => "Dialog"
        case NONE   => "None"
      }
    case None => "NA"
  }

  private var elementClassification
    : Option[ConsentDialogElementClassification] = None
  var didRejectCheck: Boolean = false
  if (!interfaceAnalysis.map(_.getSuccess).reduce((lhs, rhs) => lhs && rhs)) {
    addIssue(ConsentDialogAnalysisIssues.UNSUCCESSFUL_ANALYSIS)
  }
  assignAnalysis()
  if (minimallyComplete() && !hasBreakingIssue) {
    elementClassification = Some(
      ConsentDialogElementClassification(
        initialDialog.get.getInterfaceChain.chain match {
          case Some(chain) => chain.interface.getInterfaceElements
          case None        => List()
        },
        conf.keywordsConf,
        conf.lengthFactor
      ))
    print(elementClassification)
    identifyDialogType()
    // basic sanity check
    checkAnalysisForIssue(
      initialDialog,
      required = true,
      Nil,
      interaction = false,
      ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING,
      ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING_CHAIN,
      ConsentDialogAnalysisIssues.INITIAL_DIALOG_NO_ELEMENTS,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR
    )
    checkAnalysisForIssue(
      initialTraffic,
      required = true,
      Nil,
      interaction = false,
      ConsentDialogAnalysisIssues.INITIAL_TRAFFIC_MISSING,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR,
      ConsentDialogAnalysisIssues.THIS_AINT_NO_ERROR
    )
    // further sanity check based on the dialog type
    consentDialogType.get match {
      case LINK => // we are done now, there is only a link
        //this can happen as we are more permissive concerning buttons in our actor
        if (accept.nonEmpty || reject.nonEmpty) {}
      case NOTICE | MAYBE_NOTICE => // we are done now, there is only a notice
        if (accept.nonEmpty || reject.nonEmpty) {
          addIssue(
            ConsentDialogAnalysisIssues.NOTICE_WITH_ACCEPT_REJECT_ANALYSIS)
        }
      case NONE => //we are done now, there is no dialog
        if (accept.nonEmpty || reject.nonEmpty) {
          addIssue(ConsentDialogAnalysisIssues.NONE_WITH_ACCEPT_REJECT_ANALYSIS)
        }
      case DIALOG | MAYBE_DIALOG =>
        if (!(hasAccept || hasReject)) {
          throw new RuntimeException(
            "there is a no accept and no reject elements but we classified a dialog, this is wrong")
        }
        val classification = elementClassification.get
        checkAnalysisForIssue(
          reject,
          hasUniqueClearReject,
          classification.getClearReject.map(_.getText),
          interaction = true,
          ConsentDialogAnalysisIssues.REJECT_ANALYSIS_MISSING,
          ConsentDialogAnalysisIssues.REJECT_ANALYSIS_NO_CHAIN,
          ConsentDialogAnalysisIssues.REJECT_ANALYSIS_NO_ELEMENTS,
          ConsentDialogAnalysisIssues.REJECT_ANALYSIS_MISSING_ELEMENT,
          ConsentDialogAnalysisIssues.REJECT_ANALYSIS_STOPPED_TOO_EARLY
        )
        didRejectCheck = true
        checkAnalysisForIssue(
          accept,
          hasUniqueClearAccept,
          classification.getClearAccept.map(_.getText),
          interaction = true,
          ConsentDialogAnalysisIssues.ACCEPT_ANALYSIS_MISSING,
          ConsentDialogAnalysisIssues.ACCEPT_ANALYSIS_NO_CHAIN,
          ConsentDialogAnalysisIssues.ACCEPT_ANALYSIS_NO_ELEMENTS,
          ConsentDialogAnalysisIssues.ACCEPT_ANALYSIS_MISSING_ELEMENT,
          ConsentDialogAnalysisIssues.ACCEPT_ANALYSIS_STOPPED_TOO_EARLY
        )
    }
  }

  def hasAccept: Boolean = {
    elementClassification match {
      case Some(classification) =>
        classification.hasAccept
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }

  def hasUniqueClearAccept: Boolean = {
    elementClassification match {
      case Some(classification) =>
        classification.clearAccept.length == 1
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }

  def hasReject: Boolean = {
    elementClassification match {
      case Some(classification) =>
        classification.hasReject
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }

  def hasUniqueClearReject: Boolean = {
    elementClassification match {
      case Some(classification) =>
        classification.clearReject.length == 1
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }

  def getAcceptEquivocalJson: JsValue = {
    elementClassification match {
      case Some(classification) =>
        // if we have any accept in the classification
        if (classification.hasAccept) {
          // check if any are equivocal
          JsBoolean(classification.unclearAccept.nonEmpty)
        } else {
          // else this does not apply
          JsNull
        }
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }
  private var acceptHighlighted: Option[Boolean] = None
  def getAcceptHighlightedJson: JsValue = {
    acceptHighlighted match {
      case Some(value) => JsBoolean(value)
      case None if hasAccept && hasReject =>
        val classification = elementClassification.getOrElse(
          throw new RuntimeException("have to do element classification first"))
        acceptHighlighted = Some(
          CompareElements.elementColorHighlighted(
            classification.getAccept,
            classification.getReject,
            conf.colorDeltaEThreshold
          )
        )
        JsBoolean(acceptHighlighted.get)
      case _ => JsNull
    }
  }
  private var acceptLarger: Option[Boolean] = None
  def getAcceptLargerJson: JsValue = {
    acceptLarger match {
      case Some(value) => JsBoolean(value)
      case None if hasReject && hasAccept =>
        val classification: ConsentDialogElementClassification =
          elementClassification.getOrElse(
            throw new RuntimeException(
              "have to do element classification first")
          )
        acceptLarger = Some(
          CompareElements.elementSizeHighlighted(
            classification.getAccept,
            classification.getReject,
            conf.sizeFactor
          )
        )
        JsBoolean(acceptLarger.get)
      case _ => JsNull
    }
  }

  def getRejectEquivocalJson: JsValue = {
    elementClassification match {
      case Some(classification) =>
        // if we have a reject
        if (classification.hasReject) {
          // check if any of them were equivocal
          JsBoolean(classification.unclearReject.nonEmpty)
        } else {
          // else this does not apply
          JsNull
        }
      case None =>
        throw new RuntimeException("have to do element classification first")
    }
  }

  private var rejectStopsApp: Option[Boolean] = None
  def getRejectStopsAppJson: JsValue = {
    rejectStopsApp match {
      case Some(value)                   => JsBoolean(value)
      case None if !hasUniqueClearReject => JsNull
      case _ if consentDialogType.get == DIALOG =>
        reject match {
          case Some(rejectAnalysis) =>
            rejectStopsApp = Some(rejectAnalysis.getInterfaceChain.chain match {
              case Some(ChainEnd(_)) =>
                throw new RuntimeException(
                  s"this should have been detected prior ${app.get}")
              case Some(StoppingInteraction(_, _)) =>
                true
              case Some(NonStoppingInteraction(_, _, _)) =>
                false
              case None =>
                throw new RuntimeException(
                  s"this should have been detected prior ${app.get}")
            })
            JsBoolean(rejectStopsApp.get)
          case None =>
            throw new RuntimeException(
              s"this should have been detected prior ${app.get}")
        }
      case _ => JsNull // we are actually fine
    }
  }

  private def checkAnalysisForIssue(analysis: Option[InterfaceAnalysis],
                                    required: Boolean,
                                    requiredDialogElements: List[String],
                                    interaction: Boolean,
                                    missing: String,
                                    noChain: String,
                                    noElements: String,
                                    missingElement: String,
                                    missingInteraction: String): Unit = {
    analysis match {
      case Some(analysis) =>
        analysis.getInterfaceChain.chain match {
          case Some(chain) =>
            val initialElements = chain.interface.getInterfaceElements
            if (initialElements.isEmpty) {
              addIssue(noElements)
            } else if (!requiredDialogElements.exists(
                         elem => initialElements.exists(_.getText == elem))) {
              addIssue(missingElement)
            }
            chain match {
              case ChainEnd(_) if interaction => addIssue(missingInteraction)
              case _                          =>
            }
          case None => addIssue(noChain)
        }
      case None if required => addIssue(missing)
      case _                => // then everything is fine (None if !required)
    }
  }

  /** assigns the provided list of analysis to the corresponding attributes for
    * further processing
    *
    */
  private def assignAnalysis(): Unit = {
    if (interfaceAnalysis.length < 2) {
      addIssue(s"not enough (${interfaceAnalysis.length}) analysis done")
    } else if (interfaceAnalysis.length > 4) {
      addIssue(s"too many (${interfaceAnalysis.length}) analysis done")
    } else {
      interfaceAnalysis.foreach { analysis =>
        if (analysis.getDescription == conf.descriptors.initialDialog) {
          if (initialDialog.isEmpty) {
            initialDialog = Some(analysis)
          } else {
            addIssue(
              ConsentDialogAnalysisIssues.MULTIPLE_INITIAL_DIALOG_ANALYSIS)
          }
        } else if (analysis.getDescription == conf.descriptors.initialTraffic) {
          if (initialTraffic.isEmpty) {
            initialTraffic = Some(analysis)
          } else {
            addIssue(
              ConsentDialogAnalysisIssues.MULTIPLE_INITIAL_TRAFFIC_ANALYSIS)
          }
        } else if (analysis.getDescription == conf.descriptors.reject) {
          if (reject.isEmpty) {
            reject = Some(analysis)
          } else {
            addIssue(ConsentDialogAnalysisIssues.MULTIPLE_REJECT_ANALYSIS)
          }
        } else if (analysis.getDescription == conf.descriptors.accept) {
          if (accept.isEmpty) {
            accept = Some(analysis)
          } else {
            addIssue(ConsentDialogAnalysisIssues.MULTIPLE_ACCEPT_ANALYSIS)
          }
        } else {
          addIssue(s"unknown analysis description ${analysis.getDescription}")
        }
      }
    }
    initialDialog match {
      case Some(interfaceAnalysis) =>
        interfaceAnalysis.getInterfaceChain.chain match {
          case Some(chain) =>
            if (chain.interface.getInterfaceElements.isEmpty)
              addIssue(ConsentDialogAnalysisIssues.INITIAL_DIALOG_NO_ELEMENTS)
          case None =>
            addIssue(ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING_CHAIN)
        }
      case None =>
        addIssue(ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING)
    }
  }

  /** sorts the dialog elements and
    *
    */
  private def identifyDialogType(): Unit = {
    val classification: ConsentDialogElementClassification =
      elementClassification.getOrElse(
        throw new RuntimeException(
          "we have to do an element classification first"))
    def getKeywordScore: Int = {
      classification.keywords.length + (classification.halfKeywords.length * 0.5).toInt
    }
    consentDialogType =
      // if we have dialog text
      if (classification.dialogText.nonEmpty) {
        // and we have buttons
        if (classification.hasButtons) {
          // we assume a dialog
          Some(DIALOG)
        } else {
          // else we assume a notice
          Some(NOTICE)
        }
        // if we have sufficient keywords
      } else if ((getKeywordScore + (if (classification.linkText.nonEmpty) 1
                                     else 0)) > 3) {
        // and we have buttons
        if (classification.hasButtons) {
          // then it is probably a dialog
          Some(DIALOG)
        } else {
          // else it is still probably a notice
          Some(NOTICE)
        }
        // if everything did not match but we detected link text
      } else if (classification.linkText.nonEmpty) {
        // then it is a link
        Some(LINK)
      } else {
        // otherwise it is nothing
        Some(NONE)
      }
  }

  /** checks if the provided analysis is minimally complete
    *
    * i.e., we require at least an initial dialog analysis as well as a traffic collection
    *
    * @return
    */
  private def minimallyComplete(): Boolean = {
    val mini = initialDialog.nonEmpty && initialTraffic.nonEmpty && !hasBreakingIssue
    if (mini) {
      initialDialog.get.getInterfaceChain.chain match {
        case Some(chain) =>
          if (chain.interface.isFullyScreenshotted) {
            true
          } else {
            addIssue(
              ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING_SCREENSHOT)
            false
          }
        case None =>
          addIssue(ConsentDialogAnalysisIssues.INITIAL_DIALOG_MISSING_CHAIN)
          false
      }
    } else {
      false
    }
  }

  def isValidConsentDialogMeasurement: Boolean = {
    minimallyComplete() && !hasBreakingIssue
  }

  /**
    * {
    *   type : <STRING>,
    *   success : <BOOLEAN>,
    *   issues : [<ISSUE>,...]
    *   accept : {
    *       exists : <BOOLEAN>,
    *       equivocal : <BOOLEAN>
    *       larger : <BOOLEAN>,
    *       highlighted : <BOOLEAN>,
    *       newRequestsAfter : <INT>
    *   },
    *   reject : {
    *       exists : <BOOLEAN>,
    *       equivocal : <BOOLEAN>,
    *       closesAfter : <BOOLEAN>,
    *       newRequestsAfter : <INT>
    *   }
    *  }
    *
    * @return a JsObject that is pretty printed as specified above
    */
  def toJson: JsValue = {
    if (isValidConsentDialogMeasurement) {
      JsObject(
        "_type" -> JsString(getConsentDialogType),
        "buttonAccept" -> (if (consentDialogType.get == DIALOG)
                             JsObject(
                               "exists" -> JsBoolean(hasAccept),
                               "equivocal" -> getAcceptEquivocalJson,
                               "clear" -> JsBoolean(hasUniqueClearAccept),
                               "larger" -> getAcceptLargerJson,
                               "highlighted" -> getAcceptHighlightedJson,
                               "newRequestsAfter" -> JsNull
                             )
                           else JsNull),
        "buttonReject" -> (if (consentDialogType.get == DIALOG)
                             JsObject(
                               "exists" -> JsBoolean(hasReject),
                               "clear" -> JsBoolean(hasUniqueClearReject),
                               "equivocal" -> getRejectEquivocalJson,
                               "closesAfter" -> getRejectStopsAppJson,
                               "newRequestsAfter" -> JsNull
                             )
                           else JsNull),
        "issues" -> JsArray(
          analysisIssues.map(elem => JsString(elem)).toVector),
      )
    } else {
      JsObject(
        "_type" -> JsString(getConsentDialogType),
        "accept" -> JsNull,
        "reject" -> JsNull,
        "issues" -> JsArray(
          analysisIssues.map(elem => JsString(elem)).toVector),
      )
    }
  }

  def getAppIdString: String = app.get.toString

}

object AppConsentDialogAnalysis {}
