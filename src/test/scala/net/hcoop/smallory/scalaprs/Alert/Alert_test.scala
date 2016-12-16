package net.hcoop.smallory.scalaprs.alerts

import net.hcoop.smallory.scalaprs._

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class Alert_test extends FunSpec with Matchers {
  val tModel = ZonedDateTime.parse("2016-11-20T15:15:32Z")
    .toInstant.getEpochSecond();
  def modelsReturning(ret: Map[String, Double]): Map[String, Model] = {
    ret.map( x => ( x._1 -> StubModel(0, 0, tModel, x._2, x._2, x._2) ))
  }
  val h90 = modelsReturning(Map(("h" -> 90d),("X" -> 4d),("R" -> 100d) ))
  val h91 = modelsReturning(Map(("h" -> 91d),("X" -> 4d),("R" -> 100d) ))
  val h89 = modelsReturning(Map(("h" -> 89d),("X" -> 4d),("R" -> 100d) ))
  describe("Alert trait companion class") {
    describe("should instantiate from strings") {
      it("Can create Ionizing radiation alerts") {
        val a = Alert("radiation")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.RadiationAlert")
        assert(a.limit === (2000d +- 0.001d))
        assert(a.comparison === ">")
        assert(a.models === Vector("X"))
      }
      it("Can create generic 'h' alert with GeneralAlert") {
        val a = Alert("h > 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === ">")
        assert(a(h89) === None)
        assert(a(h90) === None)
        assert(a(h91) === Some(a.message))
      }
      it("Can use two-argument definitions") {
        val a = Alert("h 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === ">")
        assert(a(h89) === None)
        assert(a(h90) === None)
        assert(a(h91) === Some(a.message))
      }
      it("Can use greater-than definitions") {
        val a = Alert("h > 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === ">")
        assert(a(h89) === None)
        assert(a(h90) === None)
        assert(a(h91) === Some(a.message))
        val b = Alert("h gt 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "gt")
        assert(b(h89) === None)
        assert(b(h90) === None)
        assert(b(h91) === Some(b.message))
      }
      it("Can use greater than or equal to definitions") {
        val a = Alert("h >= 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === ">=")
        assert(a(h89) === None)
        assert(a(h90) === Some(a.message))
        assert(a(h91) === Some(a.message))
        val b = Alert("h => 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "=>")
        assert(b(h89) === None)
        assert(b(h90) === Some(b.message))
        assert(b(h91) === Some(b.message))
        val c = Alert("h ge 90")
        assert(c.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(c.limit === (90d +- 0.001d))
        assert(c.comparison === "ge")
        assert(c(h89) === None)
        assert(c(h90) === Some(c.message))
        assert(c(h91) === Some(c.message))
      }
      it("Can use equality definitions") {
        val a = Alert("h = 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.0001d))
        assert(a.comparison === "=")
        assert(a(h89) === None)
        assert(a(h90) === Some(a.message))
        assert(a(h91) === None)
        val b = Alert("h == 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "==")
        assert(b(h89) === None)
        assert(b(h90) === Some(b.message))
        assert(b(h91) === None)
        val c = Alert("h eq 90")
        assert(c.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(c.limit === (90d +- 0.001d))
        assert(c.comparison === "eq")
        assert(c(h89) === None)
        assert(c(h90) === Some(c.message))
        assert(c(h91) === None)
      }
      it("Can use inequality definitions") {
        val a = Alert("h != 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === "!=")
        assert(a(h89) === Some(a.message))
        assert(a(h90) === None)
        assert(a(h91) === Some(a.message))
        val b = Alert("h ne 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "ne")
        assert(b(h89) === Some(b.message))
        assert(b(h90) === None)
        assert(b(h91) === Some(b.message))
        val c = Alert("h <> 90")
        assert(c.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(c.limit === (90d +- 0.001d))
        assert(c.comparison === "<>")
        assert(c(h89) === Some(c.message))
        assert(c(h90) === None)
        assert(c(h91) === Some(c.message))
      }
      it("Can use lesser-or-equal definitions") {
        val a = Alert("h <= 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === "<=")
        assert(a(h89) === Some(a.message))
        assert(a(h90) === Some(a.message))
        assert(a(h91) === None)
        val b = Alert("h le 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "le")
        assert(b(h89) === Some(b.message))
        assert(b(h90) === Some(b.message))
        assert(b(h91) === None)
        val c = Alert("h =< 90")
        assert(c.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(c.limit === (90d +- 0.001d))
        assert(c.comparison === "=<")
        assert(c(h89) === Some(c.message))
        assert(c(h90) === Some(c.message))
        assert(c(h91) === None)
      }
      it("Can use lesser-than definitions") {
        val a = Alert("h < 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(a.limit === (90d +- 0.001d))
        assert(a.comparison === "<")
        assert(a(h89) === Some(a.message))
        assert(a(h90) === None)
        assert(a(h91) === None)
        val b = Alert("h lt 90")
        assert(a.getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.GeneralAlert")
        assert(b.limit === (90d +- 0.001d))
        assert(b.comparison === "lt")
        assert(b(h89) === Some(b.message))
        assert(b(h90) === None)
        assert(b(h91) === None)
      }
    }
  }
}

