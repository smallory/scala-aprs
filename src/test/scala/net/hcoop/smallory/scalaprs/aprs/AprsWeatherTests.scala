package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSpec}
import org.scalatest.Matchers._

class aprsWeatherTest extends FunSpec {
  describe("AprsWeather") {
    describe("when new") {
      it("should ignore weather-free messages") {
        val wx = AprsWeather("Four score and love years ago, wait... tennis?")
        assert(wx.wx.size === 0)
      }
      it("should load base wx data from uncompressed weather messages") {
        val wx = AprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
        assert(wx.wx != null)
        assert(wx.wx.size >= 4)
        assert(wx.wx("s") === 5)
        assert(wx.wx("c") === 150)
        assert(wx.wx("g") === 8)
        assert(wx.wx("t") === 26)
        val wx2 = AprsWeather("315/002g008t034r004p004P004b10335h32eMH50")
        assert(wx2.wx != null)
        assert(wx.wx.size >= 4)
        assert(wx2.wx("c") === 315)
        assert(wx2.wx("s") === 2)
        assert(wx2.wx("g") === 8)
        assert(wx2.wx("t") === 34)
      }
      ignore("should load base wx data from compressed weather messages") {
        // This is not a compressed example!
        val wx = AprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
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
        val wx = AprsWeather("150/005g008t026r000p000P000b10276h78L341.WD 31")
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
        val wx = AprsWeather("350/015g...t035h93b10122 CO_Broomfield/Jeffco")
        assert(wx.get("c").isDefined)
        assert(wx.get("s").isDefined)
        assert(wx.get("t").isDefined)
        assert(!wx.get("g").isDefined)
        assert(wx.get("t").get === 35)
        assert(wx.get("c").get === 350)
        assert(wx.get("s").get === 15)
      }
      it("should expand the radiation exponent correctly") {
        /*
         From the APRS spec:
         We chose "X" for radiation.  The xxx are like the resistor
         code.  First, two digits of precision and the last digit is the order
         of magnitude in NANOSEVERTS/Hr.  So 123 is 12 * 10^3 nanosieverts/hr or
         12 microsieverts/hr. Or 456 is 45 * 10^6 nanosieverts/hr or 45 
         millisieverts/hr.  One bananna generates about .1 uSieverts/hr, 
         a Brazil nut .4 uS/hr.

         From a post-Fukushima article in The Guardian:
         Recommended limit for radiation workers every five years: 100.00 mSv
            = 20 mSv/yr = 2283 nSv/Hr, this might be a good warning level.
         Lowest annual dose at which any increase in cancer is clearly evident: 
            100.00mSv/year =11415 nSv/Hr
         */
        val wx = AprsWeather("150/005g008t026h  X123")
        assert(wx.wx.size === 5)
        assert(wx.get("X").isDefined)
        assert(wx.get("X").get === 12000)
        assert(wx.get("c").get === 150)
        assert(wx.get("s").get === 5)
        assert(wx.get("g").get === 8)
        assert(wx.get("t").get === 26)
        assert(!wx.get("h").isDefined)
      }
      it("should handle negative temperatures and water stages correctly"){
        val wx = AprsWeather("150/005g008t-26F-.03h99")
        assert(wx.wx.size === 6)
        assert(wx.get("F").isDefined)
        assert(wx.get("h").isDefined)
        assert(wx.get("t").isDefined)
        assert(wx.get("g").isDefined)
        assert(wx.get("s").isDefined)
        assert(wx.get("c").isDefined)
        assert(wx.get("h").get === 99)
        // Finally got this working: type specification is neccessary
        assert(wx.get("F").get === (-0.03f +- 0.0001f))
        assert(wx.get("F").get + 0.03 < 0.0001)
        assert(wx.get("F").get + 0.03 > -0.0001)
        assert(wx.get("c").get === 150)
        assert(wx.get("s").get === 5)
        assert(wx.get("g").get === 8)
        assert(wx.get("t").get === -26)
      }
      it("should correctly understand h00 as being 100% humidity") {
        //METAR-2>APFMET,TCPIP*,qAC,N5JXS-F1:;MTRKBKF  *221135z3942.60N/10445.48W_020/006g...t039h00b10075 CO_Aurora, Buckley AFB Airport
        val wx = AprsWeather("020/006g...t039h00b10075 CO_Aurora, Buckley AFB Airport")
        assert(wx.wx != null)
        assert(wx.wx.size === 5)
        assert(wx.get("h").isDefined)
        assert(wx.get("h").get === 100)
        assert(wx.get("c").get === 20)
        assert(wx.get("s").get === 6)
        assert(wx.get("b").get === 10075)
        assert(wx.get("t").get === 39)
      }
      it("should add 1000 to the values in 'l' fields, and convert 'l' to 'L'") {
        val wx = AprsWeather("150/005g008t026b10276h78l341")
        assert(wx.get("h").isDefined)
        assert(wx.get("L").isDefined)
        assert(!wx.get("l").isDefined)
        assert(wx.wx.size === 7)
        assert(wx.get("b").get === 10276)
        assert(wx.get("h").get === 78)
        assert(wx.get("L").get === 1341)
        assert(wx.get("c").get === 150)
        assert(wx.get("s").get === 5)
        assert(wx.get("g").get === 8)
        assert(wx.get("t").get === 26)
      }

    }
  }
}
