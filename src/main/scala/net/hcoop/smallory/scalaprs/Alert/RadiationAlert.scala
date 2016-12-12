/**
  Alerts define the conditions that lead to a message to a user being created
  */

package net.hcoop.smallory.scalaprs.alerts

import scala.Exception

class RadiationAlert extends Alert with Serializable {
  var comparison: String = ">"
  var limit: Float = 2000.0f
  val message: String = "High ionizing radiation levels detected."
  val models: Vector[String] = Vector("X")
  def value(models: Map[String, Model]): Float = {
    return models("X").max()
  }
}

object RadiationAlert {
  def apply(setup: Array[String]): RadiationAlert = {
    val ra = new RadiationAlert()
    setup.length match {
      case 2 => try {
        ra.limit = setup(1).toFloat
      } catch { case e: Exception => {} }
      case 3 => try { 
        ra.comparison = setup(1)
        ra.limit = setup(2).toFloat
      } catch {case e: Exception => {} }
      case _ => ;
    }
    return ra
  }
}
