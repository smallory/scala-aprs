/**
  GeneralAlerts is to create an alert based on untransformed
  measurements present in the WxObservations data.
  */

package net.hcoop.smallory.scalaprs.alerts

import scala.Exception
import java.time.ZonedDateTime
import net.hcoop.smallory.scalaprs._

class GeneralAlert (valueId: String) extends Alert with Serializable {
  var comparison: String = ">"
  var compareTo: String = ""
  var limit: Double = 0.0d
  var message: String = s"Field $valueId triggered warning."
  val models: Vector[String] = Vector(valueId)

  override def value(models: Map[String, Model]): Double = {
    comparison match {
      case "<" | "<=" | "=<" | "le" | "lt" => return models(valueId).min()
      case ">" | ">=" | "=>" | "ge" | "gt" => return models(valueId).max()
      case "=" | "==" | "!=" | "<>" | "eq" | "ne" =>
        return models(valueId)(ZonedDateTime.now(utc))
    }
  }
  def updateMessage() = message = s"Field $valueId $comparison $limit."
}

object GeneralAlert {
  def apply(setup: Array[String]): GeneralAlert = {
    val ta = new GeneralAlert(setup(0))
    setup.length match {
      case 2 => try {
        ta.limit = setup(1).toDouble
      } catch {case e: Exception => logDataError(
        "Reading user file, got "+e.toString) }
      case 3 => try { 
        ta.comparison = setup(1)
        ta.limit = setup(2).toDouble
      } catch {case e: Exception => logDataError(
        "Reading user file, got "+e.toString) }
      case _ => ;
    }
    ta.updateMessage
    return ta
  }
}
