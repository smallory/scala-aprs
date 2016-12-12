/**
  Alerts define the conditions that lead to a message to a user being created
  */

package net.hcoop.smallory.scalaprs.alerts

import scala.Exception
import java.time.ZonedDateTime
import net.hcoop.smallory.scalaprs._

class GeneralAlert (dataField: String) extends Alert with Serializable {
  var comparison: String = "<"
  var limit: Float = 0.0f
  var message: String = s"Field $dataField triggered warning."
  val models: Vector[String] = Vector(dataField)

  override def value(models: Map[String, Model]): Float = {
    comparison match {
      case "<" | "<=" | "le" | "lt" => return models(dataField).min()
      case ">" | ">=" | "ge" | "gt" => return models(dataField).max()
      case "=" | "eq"   => return models(dataField)(ZonedDateTime.now(utc))
    }
  }
  def updateMessage() = message = s"Field $dataField $comparison $limit."
}

object GeneralAlert {
  def apply(setup: Array[String]): GeneralAlert = {
    val ta = new GeneralAlert(setup(0))
    setup.length match {
      case 2 => try {
        ta.limit = setup(1).toFloat
      } catch {case e: Exception => logDataError(
        "Reading user file, got "+e.toString) }
      case 3 => try { 
        ta.comparison = setup(1)
        ta.limit = setup(2).toFloat
      } catch {case e: Exception => logDataError(
        "Reading user file, got "+e.toString) }
      case _ => ;
    }
    ta.updateMessage
    return ta
  }
}