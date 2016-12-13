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
  latitude: Float,
  longitude: Float,
  modelTime: Long,
  startTime: Long
) extends Serializable {
  import net.hcoop.smallory.scalaprs.models.{Model => our}
  val lat: Float = latitude
  val lon: Float = longitude
  val buildTime: Long = modelTime
  val earliestData: Long = startTime
  val validUnits: List[String]
  var _unit: String = ""
  var radius: Float = 15f // miles

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
  def distFilter(obsLat: Float, obsLon: Float): Boolean =
    withinRadius(lat, lon, obsLat, obsLon, radius)

  // apply gives instances of the class a '(' access method.
  def apply(when: ZonedDateTime): Float =
    apply(when.toInstant.getEpochSecond())
  def apply(): Float = apply(buildTime)
  def apply(when: Long): Float
  def min(): Float
  def max(): Float
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
    lat: Float, lon: Float,
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
      //case _ => new Model()
    }
    return mm
  }

  def getZDT(seconds: Long): ZonedDateTime = Instant.ofEpochSecond(seconds).atZone(utc)

}
