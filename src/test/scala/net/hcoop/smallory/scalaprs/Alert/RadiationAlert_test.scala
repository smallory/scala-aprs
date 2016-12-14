package net.hcoop.smallory.scalaprs.alerts
import net.hcoop.smallory.scalaprs._

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class RadiationAlert_test extends FunSpec with Matchers {
  // Default test: ionizing radiation exceeds 2000 nano-Sieverts per hour
  describe("RadiationAlert") {
    describe("with default instantiation") {
      it("should return high value in default case") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("X" -> StubModel(time, lat, lon, 20, 2100, 1000))
        )
        val ra = Alert("radiation")
        assert(ra.limit === (2000d +- 0.01d))
        assert(ra.comparison === ">")
        assert(ra.value(models) === (2100d +- 0.01d))
      }
      it("should return high value even if '<' selected") {
        val time = ZonedDateTime.parse("2016-11-20T15:15:30Z[UTC]")
          .toInstant.getEpochSecond();
        def lat = 45.0d
        def lon = -102.0d
        val models: Map[String, Model] = Map(
          ("X" -> StubModel(time, lat, lon, 20, 2100, 1000))
        )
        val ra = Alert("radiation < 50")
        assert(ra.limit === (50d +- 0.01d))
        assert(ra.comparison === "<")
        assert(ra.value(models) === (2100d +- 0.01d))
      }
    }
  }
}

