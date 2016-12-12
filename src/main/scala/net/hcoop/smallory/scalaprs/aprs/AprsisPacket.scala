/*
 * Copyright 2016 Sean Mallory
 * 
 * All rights reserved until license chosen.
 * 
 */

package net.hcoop.smallory.scalaprs

import scala.collection.mutable.ArrayBuffer
import scala.Exception
//import java.time.Datetime

class AprsisPacket {
  import net.hcoop.smallory.scalaprs.{AprsisPacket => our}
  var comment: Boolean = false
  var source: String = null
  var destination: String = null
  var digipeaters: String = null
  var messageType: String = null
  var payload: String = null
  var date: AprsDate = null
  var position: AprsPosition = null
  var message: String = null
  var weather: AprsWeather = null
 // var location: Tuple2<Int, Int>
 // var time:

  // def asRecord(): Tuple4(Float, Float, Datetime, Float) = {
  //   val (fLat, fLon) = position.getFloat
  //   (fLat, fLon, time, temp)
  // }
  def readPacket(msg: String) {
    comment = our.commentP(msg)
    if (msg.length < 3) return
    if (comment) return
    val fe = (our.fieldRegex findFirstIn msg)
    fe match {
      case Some(fe) => {
        val our.fieldRegex( s, d, dd, t, p) = fe
        source = s
        destination = d
        digipeaters = dd
        messageType = t
        payload = p
      }
      case None => None
    }
    date = AprsDate(payload)
    position = AprsPosition(payload)
    message = payload drop position.timePosLength
    position.symbol match {
      // Standard weather site
      case "_" => weather = AprsWeather(message)
      // NWS weather site
      case "W" => weather = AprsWeather(message)
      // Hazards may have weather
      case "H" => weather = AprsWeather(message)
      case _ => {}
    }
  }

  def getWxObservations(): ArrayBuffer[WxObservation] = {
      //ArrayBuffer[(Float, Float, String, String, Float, String)] = {
      // var ret: ArrayBuffer[(Float, Float, String, String, Float, String)] = ArrayBuffer()
    var ret: ArrayBuffer[WxObservation] = ArrayBuffer()
    try {
      val (lat, lon) = position.position()
      // if (("null".r findFirstIn date.theDate) != None) logDebug( payload)
      val time = date.toLong
      for (oType <- "thscgrpPbL") {
        val oval = weather.getObs(oType.toString)
        if (oval != None) {
          val oo = oval.get
          ret += WxObservation(
            lat, lon, time, oo.feature, oo.value, oo.unit)
        }
      }
    } catch {
      case e: Exception => {
        logDebug(e.toString + ", " + payload)
        ret.clear
      }
    }
    return ret
  }

  def fmt(): String = {
    f"$source%10s: ($date%s : $position%s) $message%s"
  }

}

object AprsisPacket {
  val fieldRegex = """^([^>]*)>([^,:]*)([^:]*):(.)(.*)$""".r
  val commentRegex = """^ *#""".r

  var year: String = null
  var month: String = null
  var day: String = null
  var hour: String = null
  var minute: String = null

 /** Current-datastream-time information
    *  This is collected in the companion object, since it will
    *  be used to complete the time infomation on all incoming
    *  data records, and is stateful in the stream.
    *  Using a set of strings instead of Java.Time as the strings are
    *  more efficient while running the particular use case here.
    *  If we knew we were running on a realtime stream, and not a playback,
    *  we could track machine time to adjust time since last time message,
    *  and make the Java.Time infrastructure do something useful.
    */
  def streamTime(): Tuple5[String, String, String, String, String] = {
    return (year, month, day, hour, minute)
  }

  // # javAPRSSrvr 4.1.0b05 21 Nov 2016 03:18:32 GMT WE7U-F2 14580
  val serverRegex =
    """# [a-zA-Z]+ [.0-9a-z-]* (\d\d) ([A-Za-z]+) (\d\d\d\d) (\d\d):(\d\d):\d\d GMT [-A-Z0-9]+ \d+""".r
  def commentP(record: String): Boolean = {
    if (record.length < 1) return true
    serverRegex findFirstIn record match {
      case Some(str) => {
        val serverRegex(dd, mmm, yyyy, hh, mm) = str
        year = yyyy
        month = mmm
        day = dd
        hour = hh
        minute = mm
      }
      case None => {}
    }
    return commentRegex findFirstIn record match {
      case Some(x) => true
      case None => false
    }
  }

  def apply(record: String) = {
    val a = new AprsisPacket()
    a.readPacket(record)
//    if (a.messageType== "@") a.additional ++ getTimestamp(a.payload)
    a
  }

}

/**
 * APRS Data and Data Extension
 *  There are 10 main types of APRS Data:
•	Position
•	Direction Finding
•	Objects and Items
•	Weather
•	Telemetry
•	Messages, Bulletins and Announcements
•	Queries
•	Responses
•	Status
•	Other
Some of this data may also have an APRS Data Extension that provides
additional information.
The APRS Data and optional Data Extension follow the Data Type Identifier.
The table on the next page shows a complete list of all the different possible
types of APRS Data and APRS Data Extension.
  */
// object aprsisPacket {
//   def getPayload(ptxt: String): aprsData {
//     val t = ptxt take 1
//     t match {
//       // after ":", the payload format. Only "messages" can contain weather
//       // !	position without timestamp, no mesage
//       // $	Raw GPS data or Ultimeter 2000, probably NMEA formatted data
//       // _  positionless weather report
//       // =  position without timestamp with message
//       // :  message without position or timestamp
//       // ;  object - can include gages
//       // GAGE-6>APFH2O,TCPIP*,qAC,WE7U-F1:;394839104*201813z3948.60N/10457.03Ww9.27gh/18cfs
//       // @  position with message with timestamp
//       // AE5VQ>APRS,TCPIP*,qAC,T2PR:@201812z3936.00N/10521.23W_090/002g004t064r000p000P000b10293h21eMH50
//       case "@" : {
//       }
//       case _ : Null
//     }

//     val sym2 = message take 1
//     sym2 match {
//       // weather
//       case "_" : {
//       }
//       case _ : Null
//     }
//   }
// }
