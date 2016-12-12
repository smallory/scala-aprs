/**
  Alerts define the conditions that lead to a message to a user being created
  */

package net.hcoop.smallory.scalaprs.alerts

import scala.Exception

class TemperatureAlert extends Alert with Serializable {
  var comparison: String = "<"
  var limit: Float = 32.0f
  val message: String = "Freezing temperature rather likely."
  val models: Vector[String] = Vector("temperature")

  override def value(models: Map[String, Model]): Float = {
    return models("temperature").min()
  }
}

object TemperatureAlert {
  def apply(setup: Array[String]): TemperatureAlert = {
    val ta = new TemperatureAlert()
    setup.length match {
      case 2 => try {
        ta.limit = setup(1).toFloat
      } catch {case e: Exception => {} }
      case 3 => try { 
        ta.comparison = setup(1)
        ta.limit = setup(2).toFloat
      } catch {case e: Exception => {} }
      case _ => ;
    }
    return ta
  }
}