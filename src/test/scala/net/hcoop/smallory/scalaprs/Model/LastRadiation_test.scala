package net.hcoop.smallory.scalaprs.models
import java.time.{ZonedDateTime, Duration}
import org.scalatest.{FunSpec, Matchers}
import net.hcoop.smallory.scalaprs.WxObservation

class LastRadiation_test extends FunSpec with Matchers {
  val radLow: Array[WxObservation] = Array(
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "X", 5f, "nSv"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "X", 3f, "nSv"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "X", 4f, "nSv"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "X", 10f, "")
  )
  val radHigh: Array[WxObservation] = Array(
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "X", 500f, ""),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "X", 1400f, ""),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "X", 2001f, ""),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "X", 10f, "")
  )
  describe("Model") {
    it("should produce LastRadiation when passed 'X'") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = 102.0f
      val c = Model(lat, lon, "X", tModel)
      assert(c.getClass.getName ===
        "net.hcoop.smallory.scalaprs.models.LastRadiation")
    }
  }
    describe("in LastRadiation") {
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
      val c = Model(lat, lon, "X", tModel)
      assert(c.timeFilter(tModel) === true)
      assert(c.timeFilter(tBefore) === false)
      assert(c.timeFilter(tDuring) === true)
      assert(c.timeFilter(tAfter) === false)
    }
    ignore("should filter out far away obs") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = -102.0f
      def latNear = 45.0001f
      def lonNear = -101.9999f
      def latFar = -45.0001f
      def lonFar = -102.0001f
      val c = Model(lat, lon, "X", tModel)
      assert(c.distFilter(latNear, lonNear) === true)
      assert(c.distFilter(latFar, lonFar) === false)
      assert(c.distFilter(latNear, lonFar) === false)
      assert(c.distFilter(latFar, lonNear) === false)
    }
  }
}