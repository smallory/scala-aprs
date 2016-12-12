package net.hcoop.smallory.scalaprs.models
import java.time.{ZonedDateTime, Instant}

import net.hcoop.smallory.scalaprs._

class LastTemperature extends Model {
  import net.hcoop.smallory.scalaprs.models.{LastTemperature => our}
  val validUnits = our.validUnits
  _unit = "F"
  var lastObs: Float = Float.NaN
  var minObs: Float = Float.MaxValue
  var minObsTime: Long = Long.MinValue
  var maxObs: Float = Float.MinValue
  var maxObsTime: Long = Long.MinValue
  var timeLast: Long = Long.MinValue

  def addObservation(obs: WxObservation): Unit = {
    if (obs.feature != "t") return ;
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

  def doUnits(raw: Float): Float = {
    // This needs to stay consistant with our.validUnits
    // but "case our.validUnits.head => lastTemp" not a stable identifier
    return _unit match {
      case "F" => return raw // F default in APRS system
      case "C" => 5 * (raw - 32.0f) / 9
        // case "F" => 32.0f + 9 * (lastTemp / 5)
    }
  }

  def apply(when: Long): Float = doUnits(lastObs)
  override def apply(): Float = doUnits(lastObs)
  def max(): Float = doUnits(maxObs)
  def maxTime(): ZonedDateTime = Model.getZDT(maxObsTime)
  def min(): Float = doUnits(minObs)
  def minTime(): ZonedDateTime = Model.getZDT(minObsTime)

}

object LastTemperature {
  val measure = "t"
  // List default unit as first unit.
  val validUnits: List[String] = List("F", "C")
}
