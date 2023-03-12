package de.tubs.cs.ias.plotalyzer.utility.output

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object Time {

  val ZONED_DATE_TIME_FORMAT = "yyyyMMddHHmmssZ"

  def format(time: ZonedDateTime): String = {
    DateTimeFormatter.ofPattern(ZONED_DATE_TIME_FORMAT).format(time)
  }

}
