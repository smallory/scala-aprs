package net.hcoop.smallory.scalaprs.models
import java.time.{ZonedDateTime, Duration, Instant}
import org.scalatest.{FunSpec, Matchers}
import net.hcoop.smallory.scalaprs.{WxObservation, utc}

class LastTemperature_test extends FunSpec with Matchers {
    val wxa: Array[WxObservation] = Array(
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "t", 33f, "farenheit"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "t", 35f, "farenheit"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "t", 34f, "farenheit"),
    WxObservation(0f, 0f,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "t", 30f, "farenheit")
    )
  val tWxaModel = ZonedDateTime.parse("2016-11-20T15:15:32Z")
      .toInstant.getEpochSecond();
  def setupWxaModel(): Model = {
    def lat = 45.0f
    def lon = -102.0f
    val mm = Model(lat, lon, "t", tWxaModel)
    wxa.foreach(obs => mm.addObservation(obs))
    return mm
  }

  describe("Model") {
    it("should produce LastTemperature when passed 't'") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = 102.0f
      val c = Model(lat, lon, "t", tModel)
      assert(c.getClass.getName ===
        "net.hcoop.smallory.scalaprs.models.LastTemperature")
    }
  }
  describe("in LastTemperature") {
    it("should filter before and after times correctly") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
        .toInstant.getEpochSecond();
      val tBefore = ZonedDateTime.parse("2016-11-19T15:15:00Z")
        .toInstant.getEpochSecond();
      val tDuring = ZonedDateTime.parse("2016-11-19T15:17:30Z")
        .toInstant.getEpochSecond();
      val tAfter = ZonedDateTime.parse("2016-11-20T15:15:31Z")
        .toInstant.getEpochSecond();
      val lat = 45.0f
      val lon = 102.0f
      val back = Duration.ofDays(1)
      val c = Model(lat, lon, "t", tModel, back)
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
      def lonFar = -92.0001f
      val c = Model(lat, lon, "t", tModel)
      assert(c.distFilter(latNear, lonNear) === true)
      assert(c.distFilter(latFar, lonFar) === false)
      assert(c.distFilter(latNear, lonFar) === false)
      assert(c.distFilter(latFar, lonNear) === false)
    }
    describe("from a simple sequence of 't' WxObservation") {
      it("Find the last temperature, and it's time") {
        val mm = setupWxaModel()
        assert(mm() === (34f +- 0.001f))
        assert(mm(tWxaModel - 3) === (34f +- 0.001f))
      }
      it("Find the minimum temperature, and it's time") {
        val mm = setupWxaModel()
        assert(mm.min() === (30f +- 0.001f))
        assert(mm.minTime === Instant.ofEpochSecond(tWxaModel -3).atZone(utc))
      }
      it("Find the maximum temperature, and it's time") {
        val mm = setupWxaModel()
        assert(mm.max() === (35f +- 0.001f))
        assert(mm.maxTime === Instant.ofEpochSecond(tWxaModel -1).atZone(utc))
      }
      it("Find the last, min, max in degree C") {
        val mm = setupWxaModel()
        mm.unit = "C"
        assert(mm.unit === "C")
        assert(mm() === (1.1111f +- 0.001f))
        assert(mm.min() === (-1.1111f +- 0.001f))
        assert(mm.max() === (1.6666f +- 0.001f))
      }
    }
  }
}
