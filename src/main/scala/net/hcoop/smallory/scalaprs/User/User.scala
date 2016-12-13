/**
  Class representing a user at a location
  Includes the list of alerts they wish to recieve, and from there
  an array of models that provide the data that the alerts need.
  */

package net.hcoop.smallory.scalaprs

import scala.collection.mutable.{ArrayBuffer, Map => MuteMap}

class User (
  val user: String = "",
  val lat: Float = Float.NaN,
  val lon: Float = Float.NaN
) extends Serializable {
  // measurement(s), comparison, value, action
  var models: Map[String, Model] = Map()
  var alerts: List[Alert] = List()

  var whereClause: String = "WHERE 1=1"
  def addObservation(obs: WxObservation) = {
    models.map( m => {
      // remember that m is a tuple from a map
      m._2.addObservation(obs)
    })
  }

  def checkAlerts(): ArrayBuffer[String] = {
    val ret: ArrayBuffer[String] = ArrayBuffer()
    ret ++= alerts
      .map( a => {
        // TODO: make this compatible with min/max guessing Alerts
        val av = a.value(models)
        if (av != Float.NaN) a.comparison match {
          case ">"  | "gt" =>
            if (av > a.limit) Some(a.message) else None
          case ">=" | "ge" | "=>" =>
            if (av >= a.limit) Some(a.message) else None
          case "="  | "eq" | "==" =>
            if (av > a.limit) Some(a.message) else None
          case "<=" | "le" | "=<" =>
            if (av <= a.limit) Some(a.message) else None
          case "<"  | "lt" =>
            if (av < a.limit) Some(a.message) else None
        } else None
      })
      .filter(_ != None)
      .map(a => a.get)
    return ret
  }
}

object User {
  /**
    Parse location definition string:
    xxxx@example.com, lat, lon, radiation, temperature < 0, ...
    */
  def apply(defn: String): Option[User] = {
    val commentRegex = """^ *#""".r
      if (defn.length < 1) return None
    commentRegex findFirstIn defn match {
      case Some(x) => return None
      case None => {}
    }
    var tokens: Array[String] = defn.split(",")
    if (tokens.length < 3) return None

    val ll = new User(
      user = tokens(0),
      lat = AprsPosition.dddmmmmmToFloat(tokens(1)),
      lon = AprsPosition.dddmmmmmToFloat(tokens(2))
    )

    val alertsArray: ArrayBuffer[Alert] = ArrayBuffer()
    for (i <- 3 until tokens.length) {
      alertsArray += alerts.Alert(tokens(i))
    }
    ll.alerts = alertsArray.toList

    val modMap: MuteMap[String, Model] = MuteMap()
    ll.alerts.map( a => {
      a.models map ( m => {
        if (!(modMap contains m))
          modMap(m) = models.Model(ll.lat, ll.lon, m)
      })
    })
    ll.models = modMap.toMap

    return Some(ll)
  }
}
