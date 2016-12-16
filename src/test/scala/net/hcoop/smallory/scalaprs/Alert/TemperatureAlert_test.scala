package net.hcoop.smallory.scalaprs.alerts
import net.hcoop.smallory.scalaprs._

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class TemperatureAlert_test extends FunSpec with Matchers {
  // Default test: temperature < 32d
  describe("TemperatureAlert") {
    describe("with default instantiation") {
      it("should return low value in default case") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("X" -> StubModel(time, lat, lon, 2, 7, 3)),
          ("temperature" -> StubModel(time, lat, lon, 20, 80, 33))
        )
        val ra = Alert("temperature")
        assert(ra.limit === (32d +- 0.01d))
        assert(ra.comparison === "<")
        assert(ra.value(models) === (20d +- 0.01d))
      }
      it("should return high value if '>' selected") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("temperature" -> StubModel(time, lat, lon, 20, 70, 33))
        )
        val ra = Alert("temperature > 50")
        assert(ra.limit === (50d +- 0.01d))
        assert(ra.comparison === ">")
        assert(ra.value(models) === (70d +- 0.01d))
      }
      it("should allow temperature test changes without operator") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("temperature" -> StubModel(time, lat, lon, 20, 70, 33))
        )
        val ra = Alert("temperature 36")
        assert(ra.limit === (36d +- 0.01d))
        assert(ra.comparison === "<")
        assert(ra.value(models) === (20d +- 0.01d))
      }

    }
  }
}

