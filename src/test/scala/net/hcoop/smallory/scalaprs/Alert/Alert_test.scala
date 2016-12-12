package net.hcoop.smallory.scalaprs.alerts

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class RadiationAlert_test extends FunSpec with Matchers {
  describe("Alert trait companion class") {
    describe("should instantiate from strings") {
      it("Can create generic Ionizing radiiation alerts") {
        val a = Alert("radiation")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.RadiationAlert")
        assert(a.limit === (2000f +- 0.001f))
        assert(a.comparison === ">")
      }
      it("Can create generic 'h' alert with GeneralAlert") {
        val a = Alert("h > 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90f +- 0.001f))
        assert(a.comparison === ">")
      }
    }
  }
}

