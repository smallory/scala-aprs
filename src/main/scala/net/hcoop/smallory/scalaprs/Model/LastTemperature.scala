package net.hcoop.smallory.scalaprs.models
import java.time.ZonedDateTime

class LastTemperature extends Model {
  import net.hcoop.smallory.scalaprs.models.{LastTemperature => our}
  _unit = "F"
  var lastObs: Float = Float.NaN
  var minObs: Float = Float.MinValue
  var maxObs: Float = Float.MaxValue
  var timeLast: ZonedDateTime = null

  def load() {
  }
  def apply(when: ZonedDateTime = timeLast): Float = {
    // This needs to stay consistant with our.validUnits
    // but "case our.validUnits.head => lastTemp" not a stable identifier
    return _unit match {
      case "F" => return lastObs // F default in APRS system
      case "C" => 5 * (lastObs - 32.0f) / 9
      // case "F" => 32.0f + 9 * (lastTemp / 5)
    }
  }
  def max(): Float = return maxObs
  def maxTime(): ZonedDateTime = return timeLast
  def min(): Float = return minObs
  def minTime(): ZonedDateTime = return timeLast
}

object LastTemperature {
  val measure = "t"
  // List default unit as first unit.
  val validUnits: List[String] = List("F", "C")
}
