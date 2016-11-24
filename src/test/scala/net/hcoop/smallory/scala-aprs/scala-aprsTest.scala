package net.hcoop.smallory.freezewarn

import org.scalatest.{FunSuite, FlatSpec, Matchers}

class packageObjectTest extends FlatSpec with Matchers {
  "base91decode" should "return Longs from base-91" in {
    base91decode("?") should be (30)
    base91decode("S]") should be (4610)
    base91decode("<*e7") should be (20427156)
  }

  it should "create base-91 excoded Strings from Longs" in {
      assert( base91encode(32) == "A")
  }

}

class packageObjectStringTests extends FunSuite {
  test("create base-91 encoded Strings from Longs") {
      assert( base91encode(12345678) == "1Cmi")
  }
}
