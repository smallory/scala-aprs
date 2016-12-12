package net.hcoop.smallory.scalaprs.models
import java.time.ZonedDateTime

class LastRadiation extends Model {
  import net.hcoop.smallory.scalaprs.models.{LastRadiation => our}
  _unit = "nSv"
  var lastObs: Float = Float.NaN
  var minObs: Float = Float.MinValue
  var maxObs: Float = Float.MaxValue
  var timeLast: ZonedDateTime = null

  // def load(indat: RDD[(Float, Float, String, String, Float, String)]) {
  //   for (i <- indat) {
  //     val (lat, lon, time, key, value, unit) = i
  //     if (key == "X") {
  //       if (distFilter(lat, lon)) {
  //         if (timeFilter()) {
  //           if (time
  //         }
  //       }
  //     }
  //   }
  // }
  // No units checking while only one units available.
  def apply(when: ZonedDateTime = timeLast): Float =lastObs

  def max(): Float = return maxObs
  def maxTime(): ZonedDateTime = return timeLast
  def min(): Float = return minObs
  def minTime(): ZonedDateTime = return timeLast
}

object LastRadiation {
  val measure = "X"
  // List default unit as first unit.
  val validUnits: List[String] = List("nSv")
}
