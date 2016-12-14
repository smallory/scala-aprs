package net.hcoop.smallory.scalaprs.alerts

import net.hcoop.smallory.scalaprs._
import java.time.{ZoneId, ZonedDateTime}

/**
  Stubbed "Model" for testing alerts.
  */
object StubModel {
  // Default test: ionizing radiation exceeds 2000 nano-Sieverts per hour
  def apply(
    date: Long, lat: Double, lon: Double,
    low: Double, high: Double, last: Double
  ): Model = {
    return new Model(lat, lon, date, date - (24 *60 *60)) {
      val validUnits: List[String] = List("nSv")
      def apply(when: Long): Double = last
      def addObservation(obs: WxObservation) = {}
      def min(): Double = low
      def max(): Double = high
      def maxTime(): ZonedDateTime = models.Model.getZDT(date)
      def minTime(): ZonedDateTime = models.Model.getZDT(date)
    }
  }
}

