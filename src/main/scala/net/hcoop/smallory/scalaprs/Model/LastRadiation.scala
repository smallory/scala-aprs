package net.hcoop.smallory.scalaprs.models
/**
  LastRadiation is a model that merely notes recent ionizing
  radiation conditions that are reported.
  */
import java.time.ZonedDateTime
import net.hcoop.smallory.scalaprs._

class LastRadiation extends Model {
  import net.hcoop.smallory.scalaprs.models.{LastRadiation => our}
  val validUnits = our.validUnits
  _unit = "nSv"
  var lastObs: Float = Float.NaN
  var minObs: Float = Float.MinValue
  var minObsTime: Long = Long.MinValue
  var maxObs: Float = Float.MaxValue
  var maxObsTime: Long = Long.MinValue
  var timeLast: Long = Long.MinValue

  def addObservation(obs: WxObservation): Unit = {
    if (obs.feature != "X") return ;
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

  // No units checking while only one units available.
  def apply(when: Long): Float =lastObs
  def max(): Float = return maxObs
  def maxTime(): ZonedDateTime = Model.getZDT(maxObsTime)
  def min(): Float = return minObs
  def minTime(): ZonedDateTime = Model.getZDT(minObsTime) 
}

object LastRadiation {
  val measure = "X"
  // List default unit as first unit.
  val validUnits: List[String] = List("nSv")
}
