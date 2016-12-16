package net.hcoop.smallory.scalaprs

import org.scalatest.{FunSpec}
import org.scalatest.Matchers._

class aprsPositionTest extends FunSpec {
  describe("in aprsPosition object") {
    describe("expandLon") {
      it("should return 0 for empty strings") {
        assert(AprsPosition.expandLon("") === 0)
      }
      it("should pass example in spec") {
        assert(AprsPosition.expandLon("<*e7") === (-72.75d +- .001d))
      }
    }
    describe("expandLat") {
      it("should return 0 for empty strings") {
        assert(AprsPosition.expandLat("") === 0)
      }
      it("should pass example in spec") {
        // 49 30'
        assert(AprsPosition.expandLat("5L!!") === (49.5d +- .0001d))
      }
    }
    describe("methods stringLat and stringLon") {
      it("should convert Northern latitude correctly") {
        assert(AprsPosition.stringLat(90d) === "9000.00N")
        assert(AprsPosition.stringLat(45.4444444d) === "4526.67N")
      }
      it("should convert Southern latitude correctly") {
        assert(AprsPosition.stringLat(-90d) === "9000.00S")
        assert(AprsPosition.stringLat(-45.4444444d) === "4526.67S")
      }
      it("should convert Western latitude correctly") {
        assert(AprsPosition.stringLon(-90d) === "09000.00W")
        assert(AprsPosition.stringLon(-45.4444444d) === "04526.67W")
      }
      it("should convert Eastern latitude correctly") {
        assert(AprsPosition.stringLon(180d) === "18000.00E")
        assert(AprsPosition.stringLon(45.4444444d) === "04526.67E")
      }
    }
    describe("AprsPosition.dddmmmmmToDouble") {
      it("should return positive for North and East positions") {
        assert(AprsPosition.dddmmmmmToDouble("4500.00N") === (45d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("9000.00N") === (90d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("0000.00N") === (0d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("04500.00E") === (45d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("16000.00E") === (160d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("3940.96N") === (39.68267d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("10503.04W") === (-105.05067d +- 0.00001d))
      }
      it("should return positive for South and West positions") {
        assert(AprsPosition.dddmmmmmToDouble("4500.00S") === (-45d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("9000.00S") === (-90d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("0000.00S") === (0d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("04500.00W") === (-45d +- 0.00001d))
        assert(AprsPosition.dddmmmmmToDouble("16000.00W") === (-160d +- 0.00001d))
      }
      it("should return zero for unknown positions") {
        assert(AprsPosition.dddmmmmmToDouble("99999.99S") === 0)
        assert(AprsPosition.dddmmmmmToDouble("N") === 0)
        assert(AprsPosition.dddmmmmmToDouble("W") === 0)
        assert(AprsPosition.dddmmmmmToDouble("") === 0)
      }
    }
  }

  describe("aprsPosition") {
    it("should have 0N 0W when no position reported") {
      // K6DHN-9>APT314,W0UPS-5,WIDE1*,WIDE2-1,qAR,W0ARP:>TT3, K6DHN@COMCAST.NET
      val a = AprsPosition("TT3, K6DHN@COMCAST.NET")
      assert(a.lats ===  "0000.00N")
      assert(a.lons === "00000.00W")
    }
    it("should parse the position and Id the right icon when present with weather") {
      // N0LNE>APRS,TCPIP*,qAC,FIFTH:@221420z3935.04N/10510.26W_028/008g013t036r000p003P003b10110h78.WD 233
      val a = AprsPosition("221420z3935.04N/10510.26W_028/008g013t036r000p003P003b10110h78.WD 233")
      assert(a.lats === "3935.04N")
      assert(a.lons === "10510.26W")
      assert(a.symbol === "_")
      assert(a.table === "/")
      val p = a.position()
      assert(p._1 === (39.584d +- 0.00001d))
      assert(p._2 === (-105.171d +- 0.00001d))
    }
    it("should get signs right for E and S hemispheres, and find overlay values") {
      // N0LNE>APRS,TCPIP*,qAC,FIFTH:@221420z3935.04N/10510.26W_028/008g013t036r000p003P003b10110h78.WD 233
      val a = AprsPosition("221420z3935.04S110510.26E_028/008g013t036r000p003P003b10110h78.WD 233")
      assert(a.lats === "3935.04S")
      assert(a.lons === "10510.26E")
      assert(a.symbol === "_")
      assert(a.table === "1")
      val p = a.position()
      assert(p._1 === (-39.584d +- 0.00001d))
      assert(p._2 === (105.171d +- 0.00001d))
    }

    it("should be able to digest odd characters in Mic-E") {
      // K0LAI-9>SY4PTR,WIDE1-1,WIDE2-1,qAR,W0ARP:`qal v/]\"GV}449.350MHzÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ=
      val a = AprsPosition("qal v/]\"GV}449.350MHzÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ=")
      assert(a.lats ===  "0000.00N")
      assert(a.lons === "00000.00W")
    }
    it("should be able to extract position information when the timestamp is missing") {
      // N4ATA-7>APBPQ1,TCPIP*,qAC,EIGHTH:!3940.96N/10503.04WB BPQ32 Igate V 6.0.9.1
      val a = AprsPosition("3940.96N/10503.04WB BPQ32 Igate V 6.0.9.1")
      assert(a.lats === "3940.96N")
      assert(a.lons === "10503.04W")
      assert(a.symbol === "B")
      assert(a.table === "/")
      val p = a.position()
      assert(p._1 === (39.68267d +- 0.00001d))
      assert(p._2 === (-105.05067d +- 0.00001d))
    }
    it("does not recognise 'English' locations.") {
      // K0MTN-10>APWW10,TCPIP*,qAC,T2OSAKA:>DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W
      val a = AprsPosition("DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W")
      assert(a.lats ===  "0000.00N")
      assert(a.lons === "00000.00W")
    }
    it("can get lat/lon from a compressed location report") {
      // "/5L!!<*e7>7P["
      // Latitude = 49° 30' 00" north
      // Longitude = 72° 45' 00" west
      // Speed = 36.2 knots
      // Course = 88°
      val a = AprsPosition("""/5L!!<*e7>7P[""")
      assert(a.latf === (49.5d +- 0.001))
      assert(a.lonf === (-72.75d +- 0.001))
      assert(a.lats === "4930.00N")
      assert(a.lons === "07245.00W")
    }
    it("can get correct symbols and overlays from a compressed location report") {
      // "/5L!!<*e7>7P["
      // Latitude = 49° 30' 00" north
      // Longitude = 72° 45' 00" west
      // Speed = 36.2 knots
      // Course = 88°
      var a = AprsPosition("""/5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W />")
       a = AprsPosition("""a5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 0>")
       a = AprsPosition("""b5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 1>")
       a = AprsPosition("""c5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 2>")
       a = AprsPosition("""d5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 3>")
       a = AprsPosition("""e5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 4>")
       a = AprsPosition("""f5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 5>")
       a = AprsPosition("""g5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 6>")
       a = AprsPosition("""h5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 7>")
       a = AprsPosition("""i5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 8>")
       a = AprsPosition("""j5L!!<*e7>7P[""")
      assert(a.toString === "4930.00N,07245.00W 9>")
    }
      // K0XK>APDR13,TCPIP*,qAC,T2SWEDEN:=3936.47NZ10448.47W@313/000/A=005620 http://aprsdroid.org/
    // K6DHN-9>APT314,WIDE1-1,WIDE2-1,qAR,W0ARP:!3933.39N/10453.34WV115/000/A=005960!wSj!
    // K6DHN-9>APT314,WIDE1-1,WIDE2-1,qAR,W0ARP:>TT3, K6DHN@COMCAST.NET
    // W0JRL-15>APRS,TCPIP*,qAC,T2MAZURY:=3945.31N/10459.20WxPHG4270/447.175 MHz -5.00 MHz CTCSS 100.0 Hz. (AllStar Link Node 29997/EchoLink Node 201712)
    // N4ATA-7>APBPQ1,TCPIP*,qAC,EIGHTH:>BPQ32 DVRCO:N4ATA-7 Node-BBS-RMS Denver, CO
    // METAR-2>APFMET,TCPIP*,qAC,WE7U-F2:;MTRKBJC  *221415z3955.00N/10507.00W_350/015g...t035h93b10122 CO_Broomfield/Jeffco
    // N1GEP-1>S9TPSV,WIDE1-1,WIDE2-1,qAR,KC7SBS:`pIwl Wp/'"G5}!SN! 449.625 pl141.3|"+%J'e|!w;*!|3
    // # javAPRSSrvr 4.1.0b05 22 Nov 2016 14:23:39 GMT WE7U-F2 14580
    // AE5VQ>APRS,TCPIP*,qAC,T2ONTARIO:@221422z3936.00N/10521.23W_090/002g005t032r000p014P014b10201h99eMH50
  }
}
