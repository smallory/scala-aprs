package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSuite, FlatSpec, FunSpec, Matchers}
// Use Matchers to get the very handy:
//           assert(xTest === (xExpected  +- epsilon))
//
/// N.B. - this operator is very sensitive to types,
// experience so far recommends strict typing on both
// xExpected and epsilon.

/**
  These tests are trying out the "FlatSpec" format.
  */
class packageObjectTest extends FlatSpec with Matchers {
  "base91decode" should "return Longs from base-91" in {
    // "should be" is more from the Matchers object
    base91decode("?") should be (30)
    base91decode("S]") should be (4610)
    base91decode("<*e7") should be (20427156)
  }

  it should "create base-91 excoded Strings from Longs" in {
      assert( base91encode(32) === "A")
  }
}

/**
  This test repeats one above trying out the "FunSuite" format.
  */
class packageObjectFunSuiteTests extends FunSuite {
  test("create base-91 encoded Strings from Longs") {
      assert( base91encode(12345678) === "1Cmi")
  }
}

/**
  This test repeats one above trying out the "FunSpec" format.

  This seems intuitive for a structured system-subsystem-component approach,
  will use more elsewhere.
  */
class FunSpecTests extends FunSpec with Matchers {
  describe("in scala-aprs package object") {
    describe("the base91decode function"){
      it("should return correct Longs from base-91") {
        base91decode("?") should be (30)
        base91decode("S]") should be (4610)
        base91decode("<*e7") should be (20427156)
      }
    }
  }
  describe("In scala-aprs.scala") {
    describe("readArrayFromFile") {
      it("should return a map representing 'key=value' lines in a file") {
        val ll = List(
          "a=b",
          "",
          "Four score and twenty years",
          "c=Kathmandu",
          ""
        )
        val m = mapFromStrings(ll)
        assert(m.getClass.getName === "scala.collection.mutable.HashMap")
        assert(m.size === 2)
        assert(m.contains("a") === true)
        assert(m.contains("b") === false)
        assert(m.contains("c") === true)
        assert(m("a") === "b")
        assert(m("c") === "Kathmandu")
      }
    }
    describe("haversineDistance function") {
      it("should return zero for point to self distance") {
        assert( haversineDistance(
            24.345f, -111.11f, 24.345f, -111.11f, km=false) ===
          (0f +- 0.0001f))
        assert( haversineDistance(
            -60f, 1.1f, -60f, 1.1f, km=true) ===
          (0f +- 0.0001f))
      }
      it("should get example distances right") {
        assert( haversineDistance(
          36.12f,-86.67f,33.94f,-118.40f, km=true) ===
          (2887.25995f +- 0.01f))
      }
    }
    describe("withinRadius function") {
      it("should identify a point as within any radius of self") {
        assert( withinRadius(
          24.345f, -111.11f, 24.345f, -111.11f, 1f, km=false) === true)
        assert( withinRadius(
          24.345f, -111.11f, 24.345f, -111.11f, 1f, km=true) === true)
        assert( withinRadius(
            -60f, 1.1f, -60f, 1.1f, 0.01f, km=false) === true)
        assert( withinRadius(
            -60f, 1.1f, -60f, 1.1f, 0.01f, km=true) === true)
      }
      it("should differentiate between closer and further") {
        // checking around borders of example
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.2f, km=true) === false)
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.3f, km=true) === true)
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.2f, km=false) === true)
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.2f) === true)
        // Bump outside
        assert( withinRadius(
          42.12f,-86.67f,33.94f,-118.40f, 2887.31f, km=true) === false)
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.26f, km=true) === false)
        // Bump inside
        assert( withinRadius(
          36.11f,-87.67f,33.94f,-118.40f, 2887.26f, km=true) === true)
        assert( withinRadius(
          36.12f,-96.67f,33.94f,-118.40f, 2887.26f, km=true) === true)
        // change units to succeed
        assert( withinRadius(
          36.12f,-86.67f,32.94f,-118.40f, 2887.5f) === true)
        assert( withinRadius(
          36.12f,-86.67f,33.94f,-118.40f, 2887.26f) === true)
      }
    }
  }
}

/**
  Validation: the testing step where we go outside of verifying
  requirements and subsystems and corner cases and edge conditions,
  stop looking for error conditions and mis-implemented equations,
  pause the bug hunts, and instead use examples of what we expect
  and ask:
  _                    Does it work?
  */

/* Using FunSpec again, since it has provided smooth testing in scala and
 thus an effective "get it done" choice. Note, however, that "FeatureSpec"
 is recommended for this by the ScalaTest project.
*/
class validationTests extends FunSpec {
  /**
    "Freeze warning": temperatures are dropping towards 0C, and
     predicted to cross that value before warming occurs again.
    */
  describe("with DropToFreezing.dat") {
  }
  /**
    "Heat warning": temperature and humidity as combined into a
     heat index are rising towards 

    I'll need these as the testing goes deeper into the design:
    Heat index: http://www.nws.noaa.gov/om/heat/heat_index.shtml
    http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
    "Since heat index values were devised for shady, light wind conditions,
    exposure to full sunshine can increase heat index values by up to 15°F."
    */
  describe("with HeatWarningConditions.dat") {
  }
  /**
    Makin' up a "perfect" no-warning day. Based on some really fabulous
    autumn weather I have had the blessing of with experiencing. The kind
    of weather that makes peaches ripen up in the tasiest of ways.
    */
  describe("with NiceTemperatures.dat") {
  }
}
