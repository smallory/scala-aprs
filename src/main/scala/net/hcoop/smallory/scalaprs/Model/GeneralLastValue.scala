package net.hcoop.smallory.scalaprs.models
/**
  LastTemperature is a model that merely notes recent temperature
  conditions that are reported.
  */
import java.time.{ZonedDateTime, Instant}

import net.hcoop.smallory.scalaprs._

/**
  Handle min/max/last for the observation types:
  g, c, s, t, r, p, P, h, b, L, l, s, #, F, f, V, Z, and X

  No units conversions, just raw comparison.
  */
class GeneralLastValue (
  valueId: String,
  latitude: Double,
  longitude: Double,
  modelTime: Long,
  startTime: Long
) extends Model(
  latitude,
  longitude,
  modelTime,
  startTime
) {
  val validUnits = List("")
  _unit = ""
  var lastObs: Double = Double.NaN
  var minObs: Double = Double.MaxValue
  var minObsTime: Long = Long.MinValue
  var maxObs: Double = Double.MinValue
  var maxObsTime: Long = Long.MinValue
  var timeLast: Long = Long.MinValue

  override def unit: String =
    return AprsWeather.weatherFields(valueId)._2
  override def unit_= (value: String):Unit = return

  def addObservation(obs: WxObservation): Unit = {
    if (obs.id != valueId) return ;
    if (obs.time > timeLast) {
      lastObs = obs.value
      timeLast = obs.time
    };
    if (obs.value > maxObs) {
      maxObs = obs.value
      maxObsTime = obs.time
    };
    if (obs.value < minObs) {
      minObs = obs.value
      minObsTime = obs.time
    };
    return
  }

  def apply(when: Long): Double = lastObs
  override def apply(): Double = lastObs
  def max(): Double = maxObs
  def maxTime(): ZonedDateTime = Model.getZDT(maxObsTime)
  def min(): Double = minObs
  def minTime(): ZonedDateTime = Model.getZDT(minObsTime)

}
