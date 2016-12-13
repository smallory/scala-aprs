package net.hcoop.smallory.scalaprs.models
import net.hcoop.smallory.scalaprs._
import java.time.{ZonedDateTime, Duration}
import org.scalatest.{FunSpec, Matchers}

class Model_test extends FunSpec with Matchers {
  def stubModel(date: Long, lat: Float, lon: Float): Model = {
    val mm = new Model(){
      val validUnits: List[String] = List("")
      def apply(when: Long): Float = 0f
      def addObservation(obs: WxObservation) = {}
      def min(): Float = 0f
      def max(): Float = 0f
      def maxTime(): ZonedDateTime = Model.getZDT(buildTime)
      def minTime(): ZonedDateTime = Model.getZDT(buildTime)
    }
    Model.tweak(mm, lat, lon, date)
    return mm
  }
  describe("in Model") {
    it("should set start time correctly for filtering") {

      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      val dayBefore = ZonedDateTime.parse("2016-11-19T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0f
      def lon = 102.0f
      val c = stubModel(tModel, lat, lon)
      assert(c.earliestData === dayBefore)
    }
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
      val c = stubModel(tModel, lat, lon)
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
      def latFar = 25.0001f
      def lonFar = 102.0001f
      val c = stubModel(tModel, lat, lon)
      assert(c.distFilter(latNear, lonNear) === true)
      assert(c.distFilter(latFar, lonFar) === false)
      assert(c.distFilter(latNear, lonFar) === false)
      assert(c.distFilter(latFar, lonNear) === false)
    }
  }
}
