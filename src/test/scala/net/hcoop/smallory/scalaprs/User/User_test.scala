package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSuite, FlatSpec, FunSpec, Matchers}
import java.time.{ZonedDateTime}
import net.hcoop.smallory.scalaprs.models.{LastTemperature}
import net.hcoop.smallory.scalaprs.alerts.{TemperatureAlert}

class UserTests extends FunSpec with Matchers {
  val example = List(
    "one@example.com,39.584,-105.17,radiation",
    "#commented.out@example.com,39.584,-105.17,temperature",
    "two@example.com,39.584,-105.17,temperature",
    "three.four@example.com,39.584,-105.17,radiation,temperature < 20",
    "",
    "# that was a blank line to ignore",
    "temps@example.com,39.584,-105.17,temperature > 90,t = 45",
    "void@example.com,39.584,-105.17",
    "pointless@example.com,"
  )
  val wxa: Array[WxObservation] = Array(
    WxObservation(39.584d,-105.17d,
      ZonedDateTime.parse("2016-11-20T15:14:30Z").toInstant.getEpochSecond(),
      "t", 36d, "farenheit"),
    WxObservation(39.584d,-105.17d,
      ZonedDateTime.parse("2016-11-20T15:15:30Z").toInstant.getEpochSecond(),
      "t", 34d, "farenheit"),
    WxObservation(39.584d,-105.17d,
      ZonedDateTime.parse("2016-11-20T15:16:30Z").toInstant.getEpochSecond(),
      "t", 32d, "farenheit"),
    WxObservation(39.584d,-105.17d,
      ZonedDateTime.parse("2016-11-20T15:17:30Z").toInstant.getEpochSecond(),
      "t", 30d, "farenheit")
    )
  val tWxaModel = ZonedDateTime.parse("2016-11-20T15:17:32Z")
      .toInstant.getEpochSecond();
  def setupWxaUser(): User = {
    def lat = 39.584f
    def lon = -105.17f
    // "two@example.com,39.584,-105.17,temperature"
    val uu = User(example(2)).get
    wxa.foreach(obs => uu.addObservation(obs))
    return uu
  }
  def setupWxaUser6(): User = {
    def lat = 39.584f
    def lon = -105.17f
    val uu = User(example(6)).get
    wxa.foreach(obs => uu.addObservation(obs))
    return uu
  }
  describe("In User class and companion object") {
    describe("the companion apply() factory"){
      it("should return None from comments and blank lines") {
        val t2 = User(example(1))
        assert(t2 === None)
        val t5 = User(example(4))
        assert(t5 === None)
        val t6 = User(example(5))
        assert(t6 === None)
      }
      it("should return None when there are no alerts") {
        val t2 = User(example(7))
        assert(t2 === None)
        val t5 = User(example(8))
        assert(t5 === None)
      }
      it("should use the email as user identifier") {
        val t1 = User(example(0))
        assert(t1 !== None)
        var usr = t1.get
        assert(usr.user === "one@example.com")
        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        assert(usr.user === "two@example.com")
        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        assert(usr.user === "three.four@example.com")
      }
      it("should parse the text lat/lon into floats") {
        val t1 = User(example(0))
        assert(t1 !== None)
        var usr = t1.get
        assert(usr.lon === (-105.17d +- 0.001d))
        assert(usr.lat === (39.584d +- 0.0001d))
        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        assert(usr.lat === (39.584d +- 0.0001d))
        assert(usr.lon === (-105.17d +- 0.001d))
        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        assert(usr.lat === (39.584d +- 0.0001d))
        assert(usr.lon === (-105.17d +- 0.001d))
      }
      it("should create an ArrayBuffer of Alerts as named") {
        val t1 = User(example(0))
        assert(t1 !== None)
        var usr = t1.get
        usr.alerts shouldBe a [List[_]]
        assert(usr.alerts.size === 1)
        assert(usr.alerts(0).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.RadiationAlert")
        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        usr.alerts shouldBe a [scala.collection.immutable.List[_]]
        assert(usr.alerts.size === 1)
        assert(usr.alerts(0).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.TemperatureAlert")

        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        usr.alerts shouldBe a [scala.collection.immutable.List[_]]
        assert(usr.alerts.size === 2)
        assert(usr.alerts(0).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.RadiationAlert")
        assert(usr.alerts(1).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.TemperatureAlert")
      }
      it("should create an Map of Models requested by the Alerts") {
        val t1 = User(example(0))
        assert(t1 !== None)
        var usr = t1.get
        usr.models shouldBe a [scala.collection.immutable.Map[_,_]]
        assert(usr.models.size === 1)
        assert(usr.models contains "X") // from RadiationAlert
        assert(usr.models("X").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastRadiation")

        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        usr.models shouldBe a [scala.collection.immutable.Map[_,_]]
        assert(usr.models.size === 1)
        assert(usr.models contains "temperature") // from TemperatureAlert
        assert(usr.models("temperature").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastTemperature")

        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        usr.models shouldBe a [scala.collection.immutable.Map[_,_]]
        assert(usr.models.size === 2)
        assert(usr.models contains "X") // from RadiationAlert
        assert(usr.models contains "temperature") // from TemperatureAlert
        assert(usr.models("X").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastRadiation")
        assert(usr.models("temperature").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastTemperature")
      }
    }
    describe("when given WxObservations") {
      it("should load a model when given one alert calling for one model") {
        val uu = setupWxaUser()
        assert(uu.models contains "temperature")
        uu.models("temperature") shouldBe a [LastTemperature]

        assert(uu.alerts.size === 1)        
        uu.alerts(0) shouldBe a [TemperatureAlert]
        assert(uu.alerts(0).comparison === "<")
        assert(uu.alerts(0).limit === (32.0d +- 0.001d))

        assert(uu.models.size === 1)
        assert(uu.models("temperature")() === (30.0d +- 0.01d))
        assert(uu.models("temperature").max() === (36.0d +- 0.01d))
        assert(uu.models("temperature").min() === (30.0d +- 0.01d))

        val aa = uu.checkAlerts()
        assert(aa.size === 1)
        assert(aa(0) === "Freezing temperature rather likely.")
      }
      it("should load a model when given two alerts calling for same model") {
        val uu = setupWxaUser6()
        assert(uu.models contains "temperature")
        assert(uu.models.size === 1)
        uu.models("temperature") shouldBe a [LastTemperature]

        assert(uu.alerts.size === 2)
        uu.alerts(0) shouldBe a [TemperatureAlert]
        assert(uu.alerts(0).comparison === ">")
        assert(uu.alerts(0).limit === (90.0d +- 0.001d))

        uu.alerts(1) shouldBe a [TemperatureAlert]
        assert(uu.alerts(1).comparison === "=")
        assert(uu.alerts(1).limit === (45.0d +- 0.001d))

        assert(uu.models("temperature")() === (30.0d +- 0.01d))
        assert(uu.models("temperature").max() === (36.0d +- 0.01d))
        assert(uu.models("temperature").min() === (30.0d +- 0.01d))

        val aa = uu.checkAlerts()
        assert(aa.size === 0)
      }
    }
  }
}
