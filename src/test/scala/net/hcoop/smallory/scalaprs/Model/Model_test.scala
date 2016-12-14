package net.hcoop.smallory.scalaprs.models
import net.hcoop.smallory.scalaprs._
import java.time.{ZonedDateTime, Duration}
import org.scalatest.{FunSpec, Matchers}

class Model_test extends FunSpec with Matchers {
  def stubModel(date: Long, lat: Double, lon: Double): Model = {
    val mm = new Model(lat, lon, date, date - (24 *60 *60)){
      val validUnits: List[String] = List("")
      def apply(when: Long): Double = 0d
      def addObservation(obs: WxObservation) = {}
      def min(): Double = 0d
      def max(): Double = 0d
      def maxTime(): ZonedDateTime = Model.getZDT(date)
      def minTime(): ZonedDateTime = Model.getZDT(date)
    }
    return mm
  }
  describe("in Model") {
    // it("should set start time correctly for filtering") {

    //   val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
    //           .toInstant.getEpochSecond();
    //   val dayBefore = ZonedDateTime.parse("2016-11-19T15:15:30Z")
    //           .toInstant.getEpochSecond();
    //   def lat = 45.0d
    //   def lon = 102.0d
    //   val c = stubModel(tModel, lat, lon)
    //   assert(c.earliestData === dayBefore)
    // }
    it("should filter before and after times correctly") {
      val tBefore = ZonedDateTime.parse("2016-11-19T15:15:29Z")
              .toInstant.getEpochSecond();
      val tStart = ZonedDateTime.parse("2016-11-19T15:15:30Z")
              .toInstant.getEpochSecond();
      val tDuring = ZonedDateTime.parse("2016-11-19T15:17:30Z")
              .toInstant.getEpochSecond();
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      val tAfter = ZonedDateTime.parse("2016-11-20T15:15:31Z")
              .toInstant.getEpochSecond();
      def lat = 45.0d
      def lon = 102.0d
      val c = stubModel(tModel, lat, lon)
      assert(c.timeFilter(tBefore) === false)
      assert(c.timeFilter(tDuring) === true)
      assert(c.timeFilter(tModel) === true)
      assert(c.timeFilter(tAfter) === false)
      assert(c.timeFilter(tStart) === true)
    }
    it("should filter out far away obs") {
      val tModel = ZonedDateTime.parse("2016-11-20T15:15:30Z")
              .toInstant.getEpochSecond();
      def lat = 45.0d
      def lon = -102.0d
      def latNear = 45.0001d
      def lonNear = -101.9999d
      def latFar = 25.0001d
      def lonFar = 102.0001d
      val c = stubModel(tModel, lat, lon)
      assert(c.distFilter(latNear, lonNear) === true)
      assert(c.distFilter(latFar, lonFar) === false)
      assert(c.distFilter(latNear, lonFar) === false)
      assert(c.distFilter(latFar, lonNear) === false)
    }
  }
}
