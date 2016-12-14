package net.hcoop.smallory.scalaprs.alerts
import net.hcoop.smallory.scalaprs._

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class GeneralAlert_test extends FunSpec with Matchers {
  // Default test: is $"id" > 0
  describe("GeneralAlert") {
    describe("with default instantiation") {
      it("should return high value in default case") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("s" -> StubModel(time, lat, lon, -20, 100, 1))
        )
        val ra = Alert("s")
        assert(ra.models === Vector("s"))
        assert(ra.limit === (0d +- 0.01d))
        assert(ra.comparison === ">")
        assert(ra.value(models) === (100d +- 0.01d))
      }
      it("should return low value if '<' selected") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("H" -> StubModel(time, lat, lon, -20, 100, 10))
        )
        val ra = Alert("H < 50")
        assert(ra.models === Vector("H"))
        assert(ra.limit === (50d +- 0.01d))
        assert(ra.comparison === "<")
        assert(ra.value(models) === (-20d +- 0.01d))
      }
    }
  }
}

