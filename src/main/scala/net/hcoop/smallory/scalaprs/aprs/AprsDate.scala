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
  var zdt: Option[ZonedDateTime] = None

  def toLong(): Long = {
    if (zdt == None) {
      val x = this.toDate()
      if (x == null) return 0l
    }
    zdt.get.toInstant.getEpochSecond()
  }

  override def toString(): String = {
    return theDate
  }

  def toDate(): ZonedDateTime = {
    val fmt = DateTimeFormatter.ofPattern("yyyy MMM dd, kk:mm").withZone(utc)
    zdt match {
      case z: Some[ZonedDateTime] => return z.get
      case None =>
        try {
          zdt = Some(ZonedDateTime.parse(theDate, fmt))
          return zdt.get
        } catch {
          case e: DateTimeParseException => return null
          case e: java.lang.NullPointerException => return null
        }
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
            val fixedTimeRegex(d, h, m, s) = tt
            return s"$year $month $day $h:$m"
          }
          case None =>
            if (
              List(year, month, day, hour, minute)
              .filter(x => x == null).size == 0 )
              return  s"$year $month $day, $hour:$minute"
            else return null
          }
        }
    }
  }
}
