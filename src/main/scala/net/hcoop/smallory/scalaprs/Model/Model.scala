package net.hcoop.smallory.scalaprs.models
/**
  The root for classes taking recorded observations and
  variously extending, interpolating, or transforming them
  in ways that support alerts to conditions that are
  current or expected in the near future.
  */
import net.hcoop.smallory.scalaprs._

import java.time.{ZonedDateTime, Duration, Instant}

abstract class Model {
  import net.hcoop.smallory.scalaprs.models.{Model => our}
  val validUnits: List[String]
  var _unit: String = ""
  var lat: Float = 0f
  var lon: Float = 0f
  var buildTime: Long = Long.MaxValue
  var earliestData: Long = Long.MinValue
  var lookback: Duration = Duration.ofDays(1)
  var radius: Float = 15f // nautical miles

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
  def distFilter(lat: Float, lon: Float): Boolean = {
    true // TODO: tighten up distance filtering algorithm 
  }

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

  /** set values in model, so that the logic used by apply() can be tested.
    * Uses that fact that objects are passed by value. Not a function,
    * purely side-effects.
    */
  def tweak(mm: Model, lat: Float, lon: Float, time: Long) {
    tweak(mm, lat, lon, Instant.ofEpochSecond(time).atZone(utc))
  }
  def tweak(mm: Model, lat: Float, lon: Float, time: ZonedDateTime) {
    mm.lat = lat
    mm.lon = lon
    mm.buildTime = time.toInstant.getEpochSecond()
    mm.earliestData = time.minus(mm.lookback).toInstant.getEpochSecond()
  }

  /**
    Model.apply(measure) - create the default production model for a particular measure.

    New classes should not be replace the defaults used here until tests show the change
    is an upgrade to the system as a whole.
    */
  def apply(
    lat: Float, lon: Float,
    measure: String,
    time: Long = ZonedDateTime.now(utc).toInstant.getEpochSecond // Override to build models at test times
  ):Model = {
    val mm = measure match {
      case "t" | "temperature" => new LastTemperature()
      case "X" | "radiation" => new LastRadiation()
      //case _ => new Model()
    }
    tweak(mm, lat, lon, time)
    return mm
  }

  def getZDT(seconds: Long): ZonedDateTime = Instant.ofEpochSecond(seconds).atZone(utc)

}
