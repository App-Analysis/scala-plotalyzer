package de.halcony.plotalyzer.plugins

import spray.json.JsValue
import java.io.{File, FileOutputStream, PrintWriter}

/** AnalysisReturn trait to handle writing the output
  *
  * @author Simon Koch
  */
trait AnalysisReturn {

  def write(path: String): Boolean

}

/** Implementation of AnalysisReturn for JSON output
  *
  * @author Simon Koch
  *
  * @param json the json to be pretty printed
  */
case class JSONReturn(json: JsValue) extends AnalysisReturn {

  /** write the json pretty printed into the provided file URI
    *
    * @param path the path to the file to write into
    * @return true if success else false
    */
  override def write(path: String): Boolean = {
    // file either is a proper file or does not exist
    assert(new File(path).isFile || !new File(path).exists())
    val out = new PrintWriter(new FileOutputStream(new File(path)))
    try {
      out.write(json.prettyPrint)
    } finally {
      out.close()
    }
    true
  }
}
