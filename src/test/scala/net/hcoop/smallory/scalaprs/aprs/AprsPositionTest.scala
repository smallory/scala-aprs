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
        assert(AprsPosition.expandLon("<*e7") === (-72.75f +- .001f))
      }
    }
    describe("expandLat") {
      it("should return 0 for empty strings") {
        assert(AprsPosition.expandLat("") === 0)
      }
      it("should pass example in spec") {
        // 49 30'
        assert(AprsPosition.expandLat("5L!!") === (49.5f +- .0001f))
      }
    }
    describe("AprsPosition.dddmmmmmToFloat") {
      it("should return positive for North and East positions") {
        assert(AprsPosition.dddmmmmmToFloat("4500.00N") === (45f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("9000.00N") === (90f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("0000.00N") === (0f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("04500.00E") === (45f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("16000.00E") === (160f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("3940.96N") === (39.68267f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("10503.04W") === (-105.05067f +- 0.00001f))
      }
      it("should return positive for South and West positions") {
        assert(AprsPosition.dddmmmmmToFloat("4500.00S") === (-45f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("9000.00S") === (-90f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("0000.00S") === (0f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("04500.00W") === (-45f +- 0.00001f))
        assert(AprsPosition.dddmmmmmToFloat("16000.00W") === (-160f +- 0.00001f))
      }
      it("should return zero for unknown positions") {
        assert(AprsPosition.dddmmmmmToFloat("99999.99S") === 0)
        assert(AprsPosition.dddmmmmmToFloat("N") === 0)
        assert(AprsPosition.dddmmmmmToFloat("W") === 0)
        assert(AprsPosition.dddmmmmmToFloat("") === 0)
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
      assert(p._1 === (39.584f +- 0.00001f))
      assert(p._2 === (-105.171f +- 0.00001f))
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
      assert(p._1 === (39.68267f +- 0.00001f))
      assert(p._2 === (-105.05067f +- 0.00001f))
    }
    it("does not recognise 'English' locations.") {
      // K0MTN-10>APWW10,TCPIP*,qAC,T2OSAKA:>DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W
      val a = AprsPosition("DM79koI&DX: KC0D 29.9mi 125° 14:20 3922.20N 10440.76W")
      assert(a.lats ===  "0000.00N")
      assert(a.lons === "00000.00W")
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