/**
  RadiationAlert is for ionizing radiation levels reported.

  Defaults to warning at greater than 2000 nSv/hr.

  One bananna generates about .1 uSieverts/hr, a Brazil nut .4 uS/hr.

  From a post-Fukushima article in The Guardian:
  Recommended limit for radiation workers every five years: 100.00 mSv
            = 20 mSv/yr = 2283 nSv/Hr, this might be a good warning level.
  Lowest annual dose at which any increase in cancer is clearly evident: 
            100.00mSv/year =11415 nSv/Hr
  */

package net.hcoop.smallory.scalaprs.alerts

import scala.Exception

class RadiationAlert extends Alert with Serializable {
  var comparison: String = ">"
  var compareTo: String = ""
  var limit: Double = 2000.0d
  val message: String = "High ionizing radiation levels detected."
  val models: Vector[String] = Vector("X")
  def value(models: Map[String, Model]): Double = {
    return models("X").max()
  }
}

object RadiationAlert {
  def apply(setup: Array[String]): RadiationAlert = {
    val ra = new RadiationAlert()
    setup.length match {
      case 2 => try {
        ra.limit = setup(1).toDouble
      } catch { case e: Exception => {} }
      case 3 => try { 
        ra.comparison = setup(1)
        ra.limit = setup(2).toDouble
      } catch {case e: Exception => {} }
      case _ => ;
    }
    return ra
  }
}
