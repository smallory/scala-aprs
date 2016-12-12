/**
  Alerts define the conditions that lead to a message to a user being created
  */

package net.hcoop.smallory.scalaprs.alerts

trait Alert {
  def comparison: String // = ">"
  def limit: Float // = Float.NaN
  def message: String // = ""
  def models: Vector[String]
  def value(models: Map[String, Model]): Float
}

object Alert {
  def apply(lert: String):Alert = {
    val parts: Array[String] = lert.split(" ")
    return parts(0).toLowerCase match {
      case "t" | "temperature" => TemperatureAlert(parts)
      case "X" | "radiation" => RadiationAlert(parts)
      case "h"|"l"|"s"|"c"|"p"|"P" => GeneralAlert(parts)
    }
  }
}
