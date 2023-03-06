package de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request

case class TimeSpanFilter(timeSpanSec: Long) extends TrafficFilter {

  override def filter(requests: List[Request]): List[Request] = {
    implicit class RequestTimeOrder(req: Request) extends Ordered[Request] {

      override def compare(that: Request): Int =
        this.req.start.toEpochSecond.compare(that.start.toEpochSecond)
    }

    if (requests.nonEmpty) {
      val first = requests.min
      requests.filter(req =>
        req.start.toEpochSecond - first.start.toEpochSecond <= timeSpanSec)
    } else {
      List[Request]()
    }
  }
}
