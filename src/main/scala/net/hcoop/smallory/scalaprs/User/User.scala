/**
  Class for operations on the times of APRS packets

  Depends on having the data-stream time available from AprsisPacket

  */

package net.hcoop.smallory.scalaprs

import scala.collection.mutable.ArrayBuffer

class User extends Serializable {
  var user: String = ""
  var lat: Float = Float.NaN
  var lon: Float = Float.NaN
  // measurement(s), comparison, value, action
  var models: Map[String, Model] = Map()
  var alerts: ArrayBuffer[Alert] = ArrayBuffer()
}

object User {
  /**
    Parse location definition string:
    xxxx@example.com, lat, lon, radiation, temperature < 0, ...
    */
  def apply(defn: String): Option[User] = {
    val commentRegex = """^ *#""".r
    val ll = new User()
    if (defn.length < 1) return None
    commentRegex findFirstIn defn match {
      case Some(x) => return None
      case None => {}
    }
    var tokens: Array[String] = defn.split(",")
    if (tokens.length < 3) return None

    ll.user = tokens(0)
    ll.lat = AprsPosition.dddmmmmmToFloat(tokens(1))
    ll.lon = AprsPosition.dddmmmmmToFloat(tokens(2))
    for (i <- 3 until tokens.length) {
      ll.alerts += alerts.Alert(tokens(i))
    }
    return Some(ll)
  }
}
