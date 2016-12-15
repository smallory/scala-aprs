package net.hcoop.smallory.scalaprs.models

import  net.hcoop.smallory.scalaprs._
import java.time.{ZonedDateTime, Duration, Instant}
import org.scalatest.{FunSpec, Matchers}
import net.hcoop.smallory.scalaprs.WxObservation

class GeneralLastValue_test extends FunSpec with Matchers {
  val radLow: Array[WxObservation] = Array(
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "h", 5d, "nSv"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "X", 3d, "nSv"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "c", 4d, "nSv"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:33Z").toInstant.getEpochSecond(),
      "l", 1999d, "lux"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "X", 10d, "")
  )
  val radHigh: Array[WxObservation] = Array(
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "s", 500d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "h", 1400d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "c", 2001d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "X", 10d, "")
  )
  val timeModel = ZonedDateTime.parse("2016-11-20T15:15:32Z")
      .toInstant.getEpochSecond();
  def setupModel(mod: String): Model = {
    def lat = 45.0f
    def lon = -102.0f
    val mm = Model(lat, lon, "c", timeModel)
    mod match {
      case "hi" | "high" => radHigh.foreach(obs => mm.addObservation(obs))
      case "lo" | "low" => radLow.foreach(obs => mm.addObservation(obs))
      case _ => ;
    }
    return mm
  }
  describe("Model") {
    it("should produce GeneralLastValue when passed an otherwise unimplemented ID") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = 102.0f
      val c = Model(lat, lon, "h", tModel)
      assert(c.getClass.getName ===
        "net.hcoop.smallory.scalaprs.models.GeneralLastValue")
    }
  }
  describe("in GeneralLastValue") {
    it("should filter before and after times correctly") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      val tBefore = ZonedDateTime.parse("2016-11-19T15:15:00Z")
              .toInstant.getEpochSecond();
      val tDuring = ZonedDateTime.parse("2016-11-19T15:17:30Z")
              .toInstant.getEpochSecond();
      val tAfter = ZonedDateTime.parse("2016-11-20T15:15:31Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = 102.0f
      val c = Model(lat, lon, "h", tModel)
      assert(c.timeFilter(tModel) === true)
      assert(c.timeFilter(tBefore) === false)
      assert(c.timeFilter(tDuring) === true)
      assert(c.timeFilter(tAfter) === false)
    }
    it("should filter out far away obs") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = -102.0f
      def latNear = 45.0001f
      def lonNear = -101.9999f
      def latFar = -45.0001f
      def lonFar = -82.0001f
      val c = Model(lat, lon, "h", tModel)
      assert(c.distFilter(latNear, lonNear) === true)
      assert(c.distFilter(latFar, lonFar) === false)
      assert(c.distFilter(latNear, lonFar) === false)
      assert(c.distFilter(latFar, lonNear) === false)
    }
    describe("from a mixed sequence of WxObservation") {
      it("Find the last radiation obs, and it's time") {
        val mm = setupModel("low")
        assert(mm() === (4d +- 0.001d))
        assert(mm(timeModel - 3) === (4d +- 0.001d))
      }
      it("Find the minimum radiation obs, and it's time") {
        val mm = setupModel("low")
        assert(mm.min() === (4d +- 0.001d))
        assert(mm.minTime === Instant.ofEpochSecond(timeModel -0).atZone(utc))
      }
      it("Find the maximum radiation obs, and it's time") {
        val mm = setupModel("low")
        assert(mm.max() === (4d +- 0.001d))
        assert(mm.maxTime === Instant.ofEpochSecond(timeModel - 0).atZone(utc))
      }
    }
  }
}
