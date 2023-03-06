package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

object ConsentDialogAnalysisIssues {

  val THIS_AINT_NO_ERROR = "this ain't no error"

  val MULTIPLE_INITIAL_DIALOG_ANALYSIS = "multiple initial dialog analysis"
  val MULTIPLE_INITIAL_TRAFFIC_ANALYSIS = "multiple initial traffic analysis"
  val MULTIPLE_REJECT_ANALYSIS = "multiple reject analysis"
  val MULTIPLE_ACCEPT_ANALYSIS = "multiple accept analysis"

  val MULTIPLE_APPS =
    "multiple different apps contained in the provided analysis"

  val UNSUCCESSFUL_ANALYSIS = "there is an unsuccessful analysis"

  val INITIAL_DIALOG_MISSING = "initial dialog analysis is missing"
  val INITIAL_DIALOG_NO_ELEMENTS =
    "initial dialog analysis did not retrieve any elements"
  val INITIAL_DIALOG_MISSING_CHAIN =
    "initial dialog analysis is missing interface chain"
  val INITIAL_DIALOG_MISSING_SCREENSHOT =
    "initial dialog analysis is missing screenshots"

  val INITIAL_TRAFFIC_MISSING = "initial traffic analysis is missing"

  val REJECT_ANALYSIS_MISSING =
    "clear REJECT button but no corresponding analysis"
  val REJECT_ANALYSIS_NO_CHAIN = "reject analysis has no interface chain"
  val REJECT_ANALYSIS_NO_ELEMENTS = "reject analysis has no interface elements"
  val REJECT_ANALYSIS_MISSING_ELEMENT =
    "reject analysis is missing mandatory interface element"
  val REJECT_ANALYSIS_STOPPED_TOO_EARLY =
    "reject analysis stopped prior to interaction"

  val ACCEPT_ANALYSIS_MISSING =
    "clear ACCEPT button but no corresponding analysis"
  val ACCEPT_ANALYSIS_NO_CHAIN = "accept analysis has no interface chain"
  val ACCEPT_ANALYSIS_NO_ELEMENTS = "accept analysis has no interface elements"
  val ACCEPT_ANALYSIS_MISSING_ELEMENT =
    "accept analysis is missing mandatory interface element"
  val ACCEPT_ANALYSIS_STOPPED_TOO_EARLY =
    "reject analysis stopped prior to interaction"

  val NOTICE_WITH_ACCEPT_REJECT_ANALYSIS =
    "dialog is of type NOTICE but we have accept and/or reject analysis"
  val NONE_WITH_ACCEPT_REJECT_ANALYSIS =
    "dialog is of type NONE but we have accept and/or reject analysis"

}
