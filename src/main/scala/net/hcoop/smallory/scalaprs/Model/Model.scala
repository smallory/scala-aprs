package net.hcoop.smallory.scalaprs.models
/**
  The root for classes taking recorded observations and
  variously extending, interpolating, or transforming them
  in ways that support alerts to conditions that are
  current or expected in the near future.
  */
import net.hcoop.smallory.scalaprs._

import java.time.{ZonedDateTime, Duration, Instant}

abstract class Model (
  latitude: Double,
  longitude: Double,
  modelTime: Long,
  startTime: Long
) extends Serializable {
  import net.hcoop.smallory.scalaprs.models.{Model => our}
  val lat: Double = latitude
  val lon: Double = longitude
  val buildTime: Long = modelTime
  val earliestData: Long = startTime
  val validUnits: List[String]
  var _unit: String = ""
  var radius: Double = 15d // miles

  def addObservation(obs: WxObservation): Unit

  def unit: String = return _unit
  def unit_= (value: String):Unit = {
    if (validUnits contains value) _unit = value
    else logCodeError("Unit "+value+" appears to be invalid in this context")
  }

  /** Test to see if a data record is in the time window this model uses.
    * Filtering main stream, so keep it fast.
    */
  def timeFilter(date: Long): Boolean = {
    if (buildTime < date) false
    else if (earliestData > date) false
    else true
  }
  /** Test to see if a data record is in the region this model uses.
    * Filtering main stream, so keep it fast.
    */
  def distFilter(obsLat: Double, obsLon: Double): Boolean =
    withinRadius(lat, lon, obsLat, obsLon, radius)

  // apply gives instances of the class a '(' access method.
  def apply(when: ZonedDateTime): Double =
    apply(when.toInstant.getEpochSecond())
  def apply(): Double = apply(buildTime)
  def apply(when: Long): Double
  def min(): Double
  def max(): Double
  def maxTime(): ZonedDateTime
  def minTime(): ZonedDateTime
}

object Model {
  val measure: String = ""
  // List default unit as first unit.
  val validUnits: List[String] = List("")

  /**
    Model.apply(measure) - create the default production model for a particular measure.

    New classes should not be replace the defaults used here
    until tests show the change is an upgrade to the system
    as a whole.
    */
  def apply(
    lat: Double, lon: Double,
    measure: String,
    // Override the following to build models at test times
    time: Long = ZonedDateTime.now(utc).toInstant.getEpochSecond,
    lookback: Duration = Duration.ofDays(1) 
  ):Model = {
    val zBuildTime = Instant.ofEpochSecond(time).atZone(utc)
    val tStart = zBuildTime.minus(lookback)
      .toInstant.getEpochSecond()
    val mm = measure match {
      case "t" | "temperature" =>
        new LastTemperature(lat, lon, time, tStart)
      case "X" | "radiation" =>
        new LastRadiation(lat, lon, time, tStart)
      case "g"|"c"|"s"|"t"|"r"|"p"|"P"|"h"|"b"|"L"|"l"|"s"|"#"|"F"|"f"|"V"|"Z"|"X" =>
        new GeneralLastValue( measure, lat, lon, time, tStart)
    }
    return mm
  }

  def getZDT(seconds: Long): ZonedDateTime = Instant.ofEpochSecond(seconds).atZone(utc)

}
