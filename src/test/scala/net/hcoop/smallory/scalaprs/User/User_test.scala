package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSuite, FlatSpec, FunSpec, Matchers}

class UserTests extends FunSpec with Matchers {
  val example = List(
    "one@example.com,39.584,-105.17,radiation",
    "#commented.out@example.com,39.584,-105.17,temperature",
    "two@example.com,39.584,-105.17,temperature",
    "three.four@example.com,39.584,-105.17,radiation,temperature < 20",
    "",
    "# that was a blank line to ignore"
  )
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
        assert(usr.lon === (-105.17f +- 0.001f))
        assert(usr.lat === (39.584f +- 0.0001f))
        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        assert(usr.lat === (39.584f +- 0.0001f))
        assert(usr.lon === (-105.17f +- 0.001f))
        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        assert(usr.lat === (39.584f +- 0.0001f))
        assert(usr.lon === (-105.17f +- 0.001f))
      }
      it("should create an ArrayBuffer of Alerts as named") {
        val t1 = User(example(0))
        assert(t1 !== None)
        var usr = t1.get
        assert(usr.alerts.getClass.getName ===
          "scala.collection.mutable.ArrayBuffer")
        assert(usr.alerts.size === 1)
        assert(usr.alerts(0).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.RadiationAlert")

        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        assert(usr.alerts.getClass.getName ===
          "scala.collection.mutable.ArrayBuffer")
        assert(usr.alerts.size === 1)
        assert(usr.alerts(0).getClass.getName ===
          "net.hcoop.smallory.scalaprs.alerts.TemperatureAlert")

        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        assert(usr.alerts.getClass.getName ===
          "scala.collection.mutable.ArrayBuffer")
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
        assert(usr.models.getClass.getName ===
          "scala.collection.mutable.HashMap")
        assert(usr.models.size === 1)
        assert(usr.models contains "X") // from RadiationAlert
        assert(usr.models("X").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastRadiation")

        val t3 = User(example(2))
        assert(t3 !== None)
        usr = t3.get
        assert(usr.models.getClass.getName ===
          "scala.collection.mutable.HashMap")
        assert(usr.models.size === 1)
        assert(usr.models contains "temperature") // from TemperatureAlert
        assert(usr.models("temperature").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastTemperature")

        val t4 = User(example(3))
        assert(t4 !== None)
        usr = t4.get
        assert(usr.models.getClass.getName ===
          "scala.collection.mutable.HashMap")
        assert(usr.models.size === 2)
        assert(usr.models contains "X") // from RadiationAlert
        assert(usr.models contains "temperature") // from TemperatureAlert
        assert(usr.models("X").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastRadiation")
        assert(usr.models("temperature").getClass.getName ===
          "net.hcoop.smallory.scalaprs.models.LastTemperature")
      }
    }
  }
}
