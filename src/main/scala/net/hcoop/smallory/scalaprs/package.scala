/**
  Package utilities
  */
package net.hcoop.smallory
import java.time.{ZoneOffset, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle.{FULL, LONG, MEDIUM, SHORT}
import scala.collection.mutable.Map

package object scalaprs{
  type ObservationMap = scala.collection.mutable.Map[String, Float]
  type Alert = net.hcoop.smallory.scalaprs.alerts.Alert
  type Model = net.hcoop.smallory.scalaprs.models.Model

  val utc = ZoneOffset.UTC

  // Making certain that it's easy to change loggers.
  var logIgnore: List[String] = List()
  def logAs(errType: String, message: String) = {
    if (!logIgnore.contains(errType))
      println( errType.toUpperCase + " @ " +
        ZonedDateTime.now.format(
          DateTimeFormatter.ofLocalizedDateTime(SHORT, SHORT)) +
        ":" + message)
  }
  val logNote = logAs("note", _: String)
  val logInfo = logAs("note", _: String)
  val logWarn = logAs("warning", _: String)
  val logCodeError = logAs("error", _: String)
  val logDataError = logAs("data", _: String)
  val logDebug = logAs("debug", _: String)

  def base91decode(str: String): Long = {
    var l: Long = 0
    for (c <- str) {
      l *= 91
      l += c - 33
    }
    return l
  }

  def base91encode(num: Long): String = {
    var s: String = ""
    var n: Long = num
    while (n > 0) {
      s = ((n % 91) + 33).toChar + s
      n = n/91
    }
    return s
  }

  def mapFromStrings(arFile: Iterable[String]): Map[String, String] = {
    var rtrn: Map[String, String] = Map()
    rtrn ++= arFile map { (entry: String) =>
      val field = entry.split("=")
      if (field.size > 1) Some(field(0) -> field(1))
      else None
    } collect ({case Some((x:String, y:String)) => (x -> y)})
    return rtrn
  }

  /**
    withinRadius returns whether or not two points are within
    a particular radius of each other.

    This implements the haversine funtion separately from 
    haversineDistance() in order to allow various shortcut exits
    to speed up filtering large sets of observations.
    */
  def withinRadius(
    firstLat: Float, firstLon: Float,
    secondLat: Float, secondLon: Float,
    dist: Float, km: Boolean = false
  ): Boolean = {
    import scala.math.{min, cos, max, abs, toRadians, pow, sin, asin, sqrt}
    val radius = if (km) 6372.8 else 3959.0
    val rFirstLat = firstLat.toRadians
    val rSecondLat = secondLat.toRadians

    // The fastest screen
    val dLat = (rFirstLat - rSecondLat)
    if (abs(dLat) * radius > dist ) return false

    val cosLat1 = cos(rFirstLat)
    val cosLat2 = cos(rSecondLat)

    // The second fastest screen
    val dLon = (firstLon - secondLon).toRadians
    val shrink = min(cosLat1, cosLat2)
    val pdLon = if (dLon < 0) -dLon else dLon
    if ((if (pdLon > 180) 360 - pdLon else pdLon) * shrink * radius > dist )
      return false

    // The precision screen (haversine calculation)
    val radDist = 2 * asin( sqrt( pow(sin(dLat/2),2) +
      pow(sin(dLon/2),2) * cosLat1 * cosLat2 ))
    if (radDist * radius > dist) return false
    else return true
  }

  /**
    haversineDistance returns the distance between two points.
    */
  def haversineDistance(
    firstLat: Float, firstLon: Float,
    secondLat: Float, secondLon: Float,
    km: Boolean = false
  ): Float = {
    import scala.math.{Pi, cos, max, abs, toRadians, pow, sin, asin, sqrt}
    val radius = if (km) 6372.8 else 3959.0

    val rLat1 = firstLat.toRadians
    val rLat2 = secondLat.toRadians
    val dLat = (rLat1 - rLat2)
    val cosLat1 = cos(rLat1)
    val cosLat2 = cos(rLat2)
    val dLon = (firstLon - secondLon).toRadians
    val radDist = 2 * asin( sqrt( pow(sin(dLat/2),2) +
      pow(sin(dLon/2),2) * cosLat1 * cosLat2 ))
    return (radDist * radius).toFloat
  }

}

