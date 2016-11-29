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
class packageObjectFunSpecTests extends FunSpec with Matchers {
  describe("in scala-aprs package object") {
    describe("the base91decode function"){
      it("should return correct Longs from base-91") {
        base91decode("?") should be (30)
        base91decode("S]") should be (4610)
        base91decode("<*e7") should be (20427156)
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
