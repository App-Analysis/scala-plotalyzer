package de.halcony.plotalyzer.plugins

/** Trait that has to be implemented to become a plugin for the plotalyzer
  *
  */
trait AnalysisPlugin {

  /** this method starts the analysis provided with the AnlysisContext
    *
    * no rerun is expected, i.e., the analysis can store state and will not be repeated
    *
    * @param context the analysis context the analysis runs in
    * @return the Analysis return in case of success or an exception
    */
  def analyze(context: AnalysisContext): Either[Exception, AnalysisReturn]

}
