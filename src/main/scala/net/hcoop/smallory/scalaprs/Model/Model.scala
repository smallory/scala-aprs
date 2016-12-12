package net.hcoop.smallory.scalaprs.models

import net.hcoop.smallory.scalaprs.utc

import java.time.{ZonedDateTime, Duration}

abstract class Model {
  import net.hcoop.smallory.scalaprs.models.{Model => our}
  var _unit: String = ""
  var lat: Float = 0f
  var lon: Float = 0f
  var buildTime: ZonedDateTime = null
  var earliestData: ZonedDateTime = null
  var lookback: Duration = Duration.ofDays(1)
  var radius: Float = 15f // nautical miles

  def unit: String = return _unit
  def unit_= (value: String):Unit = {
    if (our.validUnits contains value) _unit = value
  }

  /** Test to see if a data record is in the time window this model uses.
    * Filtering main stream, so keep it fast.
    */
  def timeFilter(date: ZonedDateTime): Boolean = {
    if (buildTime.isBefore(date)) false
    else if (earliestData.isAfter(date)) false
    else true
  }
  /** Test to see if a data record is in the region this model uses.
    * Filtering main stream, so keep it fast.
    */
  def distFilter(lat: Float, lon: Float): Boolean = {
    true // TODO: tighten up distance filtering algorithm 
  }

  // apply gives instances of the class a '(' access method.
  def apply(when: ZonedDateTime): Float
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
  def tweak(mm: Model, lat: Float, lon: Float, time: ZonedDateTime) {
    mm.lat = lat
    mm.lon = lon
    mm.buildTime = time
    mm.earliestData = time.minus(mm.lookback)
  }

  /**
    Model.apply(measure) - create the default production model for a particular measure.

    New classes should not be replace the defaults used here until tests show the change
    is an upgrade to the system as a whole.
    */
  def apply(
    lat: Float, lon: Float,
    measure: String,
    time: ZonedDateTime = ZonedDateTime.now(utc) // Override to build models at test times
  ):Model = {
    val mm = measure match {
      case "t" | "temperature" => new LastTemperature()
      case "X" | "radiation" => new LastRadiation()
      //case _ => new Model()
    }
    tweak(mm, lat, lon, time)
    return mm
  }
}
