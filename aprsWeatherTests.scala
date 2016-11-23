package net.hcoop.smallory.freezewarn

import org.scalatest.{FunSpec}

class aprsWeatherTest extends FunSpec {
  describe("aprsWeather") {
    describe("when new") {
      it("should ignore weather-free messages") {
        val wx = aprsWeather("Four score and love years ago, wait... tennis?")
        assert(wx.wx.size === 0)
      }
      it("should load base wx data from uncompressed weather messages") {
        val wx = aprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
        assert(wx.wx != null) 
        assert(wx.wx("s") === 5)
        assert(wx.wx("c") === 150)
        assert(wx.wx("g") === 8)
        assert(wx.wx("t") === 26)
        val wx2 = aprsWeather("315/002g008t034r004p004P004b10335h32eMH50")
        assert(wx2.wx != null)
        assert(wx2.wx("c") === 315)
        assert(wx2.wx("s") === 2)
        assert(wx2.wx("g") === 8)
        assert(wx2.wx("t") === 34)
      }
      ignore("should load base wx data from compressed weather messages") {
        // This is not a compressed example!
        val wx = aprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
        assert(wx.wx != null)
        assert(wx.wx.contains("c"))
        assert(wx.wx.contains("s"))
        assert(wx.wx.contains("g"))
        assert(wx.wx.contains("t"))
        assert(wx.get("c").get === 150)
        assert(wx.get("s").get === 5)
        assert(wx.get("g").get === 8)
        assert(wx.get("t").get === 26)
      }
      it("should load extended weather data when present") {
        val wx = aprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
        assert(wx.get("r").isDefined)
        assert(wx.get("p").isDefined)
        assert(wx.get("P").isDefined)
        assert(wx.get("b").isDefined)
        assert(wx.get("h").isDefined)
        assert(wx.get("L").isDefined)
        assert(wx.wx.size === 10)
        assert(wx.get("r").get === 0)
        assert(wx.get("p").get === 0)
        assert(wx.get("P").get === 0)
        assert(wx.get("b").get === 10276)
        assert(wx.get("h").get === 78)
        assert(wx.get("L").get === 341)
        assert(wx.get("c").get === 150)
        assert(wx.get("s").get === 5)
        assert(wx.get("g").get === 8)
        assert(wx.get("t").get === 26)
      }
      it("should handle missing fields approriately in base data") {
        // METAR-2>APFMET,TCPIP*,qAC,WE7U-F2:;MTRKBJC  *221415z3955.00N/10507.00W_350/015g...t035h93b10122 CO_Broomfield/Jeffco
        val wx = aprsWeather("350/015g...t035h93b10122 CO_Broomfield/Jeffco")
        assert(wx.get("c").isDefined)
        assert(wx.get("s").isDefined)
        assert(!wx.get("g").isDefined)
        assert(wx.get("t").isDefined)
        assert(wx.get("t").get === 35)
      }
      ignore("should find standard and extended data in 'H'azard objects") {}
      ignore("should expand the radiation exponent correctly") {}
      ignore("should handle negative temperatures and water stages correctly"){}
      ignore("should correctly understand h00 as being 100% humidity") {}
      ignore("should add 1000 to the values in 'l' fields, and convert 'l' to 'L'") {}
    }
  }
}
