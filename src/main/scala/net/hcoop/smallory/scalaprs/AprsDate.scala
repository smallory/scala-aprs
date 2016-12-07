/**
  Class for operations on the times of APRS packets

  Depends on having the data-stream time available from AprsisPacket

  */

package net.hcoop.smallory.scalaprs

import java.time.ZonedDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}

class AprsDate {
  import net.hcoop.smallory.scalaprs.{AprsDate => our}
  var theDate: String = null

  override def toString(): String = {
    return theDate
  }

  def asDate(): ZonedDateTime = {
    val fmt = DateTimeFormatter.ofPattern("yyyy MMM dd, kk:mm").withZone(utc)
    try {
      return ZonedDateTime.parse(theDate, fmt)
    } catch {
      case e: DateTimeParseException => return null
    }
    return null
  }
}
object AprsDate {
  val zTimeRegex = """(\d\d)(\d\d)(\d\d)([z/h])""".r
  val fixedTimeRegex = """(\d\d)(\d\d)(\d\d)(\d\d)""".r

  def apply(payload: String) = {
    val d = new AprsDate()
    d.theDate = parseDate(payload)
    d
  }

  def parseDate(payload: String): String = {
    val (year, month, day, hour, minute) = AprsisPacket.streamTime()
    zTimeRegex findFirstIn payload match {
      case Some(z) => {
        val zTimeRegex(d, h, m, t) = z
        if ( t == "z" ) return s"$year $month $d, $h:$m"
        else if (t == "h") return s"$year $month $day, $d:$h"
        else return  s"$year $month $day, $hour:$minute-ish" // shouldn't reach
      }
      case None => {
        fixedTimeRegex findFirstIn payload match {
          case Some(tt) => {
            println(payload)
            val fixedTimeRegex(d, h, m, s) = tt
            return s"$year $month $day $h:$m"
          }
          case None => return  s"$year $month $day, $hour:$minute"
          }
        }
    }
  }
}
