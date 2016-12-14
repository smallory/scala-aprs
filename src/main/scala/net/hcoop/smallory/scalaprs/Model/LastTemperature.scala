package net.hcoop.smallory.scalaprs.models
/**
  LastTemperature is a model that merely notes recent temperature
  conditions that are reported.
  */
import java.time.{ZonedDateTime, Instant}

import net.hcoop.smallory.scalaprs._

class LastTemperature (
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
  import net.hcoop.smallory.scalaprs.models.{LastTemperature => our}
  val validUnits = our.validUnits
  _unit = "F"
  var lastObs: Double = Double.NaN
  var minObs: Double = Double.MaxValue
  var minObsTime: Long = Long.MinValue
  var maxObs: Double = Double.MinValue
  var maxObsTime: Long = Long.MinValue
  var timeLast: Long = Long.MinValue

  def addObservation(obs: WxObservation): Unit = {
    if (obs.id != "t") return ;
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

  def doUnits(raw: Double): Double = {
    // This needs to stay consistant with our.validUnits
    // but "case our.validUnits.head => lastTemp" not a stable identifier
    return _unit match {
      case "F" => return raw // F default in APRS system
      case "C" => 5 * (raw - 32.0d) / 9
        // case "F" => 32.0d + 9 * (lastTemp / 5)
    }
  }

  def apply(when: Long): Double = doUnits(lastObs)
  override def apply(): Double = doUnits(lastObs)
  def max(): Double = doUnits(maxObs)
  def maxTime(): ZonedDateTime = Model.getZDT(maxObsTime)
  def min(): Double = doUnits(minObs)
  def minTime(): ZonedDateTime = Model.getZDT(minObsTime)

}

object LastTemperature {
  val measure = "t"
  // List default unit as first unit.
  val validUnits: List[String] = List("F", "C")
}
