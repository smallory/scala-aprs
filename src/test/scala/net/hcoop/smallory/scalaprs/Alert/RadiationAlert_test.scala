package net.hcoop.smallory.scalaprs.alerts
import net.hcoop.smallory.scalaprs._

import org.scalatest.{FunSpec, Matchers}
import java.time.{ZoneId, ZonedDateTime}

class RadiationAlert_test extends FunSpec with Matchers {
  // Default test: ionizing radiation exceeds 2000 nano-Sieverts per hour
  val radLow: Array[WxObservation] = Array(
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "X", 5d, "nSv"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "X", 3d, "nSv"),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "X", 4d, "nSv"),
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
      "X", 500d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:31Z").toInstant.getEpochSecond(),
      "X", 1400d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:32Z").toInstant.getEpochSecond(),
      "X", 2001d, ""),
    WxObservation(0d, 0d,
      ZonedDateTime.parse("2016-11-20T15:15:29Z").toInstant.getEpochSecond(),
      "X", 10d, "")
  )
  val timeModel = ZonedDateTime.parse("2016-11-20T15:15:32Z")
      .toInstant.getEpochSecond();
  def setupModel(mod: String): Model = {
    def lat = 45.0f
    def lon = -102.0f
    val mm = models.Model(lat, lon, "X", timeModel)
    mod match {
      case "hi" | "high" => radHigh.foreach(obs => mm.addObservation(obs))
      case "lo" | "low" => radLow.foreach(obs => mm.addObservation(obs))
      case _ => ;
    }
    return mm
  }
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
    describe("No warning with default and low rad levels."){
      val ra = Alert("radiation")
      val mm: Map[String, Model] = Map(
        ("X" -> setupModel("low")),
        ("decoy" -> setupModel("high"))
      )
      assert(ra(mm) === None)
    }
    describe("A warning with default and high rad levels."){
      val ra = Alert("radiation")
      val mm: Map[String, Model] = Map(
        ("decoy" -> setupModel("low")),
        ("X" -> setupModel("high"))
      )
      val mess = ra(mm)
      assert(mess !== None)
      mess shouldBe a [Option[_]]
      val msg = mess.get
      assert(msg === ra.message)
    }
    describe("Warning at low rad levels with 7 nSv test."){
      val ra = Alert("radiation 7")
      val mm: Map[String, Model] = Map(
        ("X" -> setupModel("low")),
        ("decoy" -> setupModel("high"))
      )
      val mess = ra(mm)
      assert(mess !== None)
      val msg = mess.get
      assert(msg === ra.message)
    }
    describe("No warning with high rad levels and less than or eq 9.9 ."){
      val ra = Alert("radiation le 9.9")
      val mm: Map[String, Model] = Map(
        ("decoy" -> setupModel("low")),
        ("X" -> setupModel("high"))
      )
      val mess = ra(mm)
      assert(ra(mm) === None)
    }
  }
}

