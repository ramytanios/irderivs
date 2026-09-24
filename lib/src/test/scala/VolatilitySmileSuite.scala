package lib

class VolatilitySmileSuite extends munit.FunSuite:

  test("asymptotic behavior"):
    { // right slope is negative
      val ks = Vector(0.0, 0.01, 0.02, 0.04, 0.05)
      val vs = Vector(0.9, 0.6, 0.55, 0.4, 0.35)
      val smile = VolatilitySmile(ks, vs)
      val vR = vs.last
      val kInf = 10
      assertEqualsDouble(smile(kInf), vR * 2.0 / 3.0, 1e-10)
    }

    { // left slope is negative
      val ks = Vector(0.0, 0.01, 0.02, 0.04, 0.05)
      val vs = Vector(0.9, 0.6, 0.55, 0.4, 0.35).reverse
      val smile = VolatilitySmile(ks, vs)
      val vL = vs.head
      val kMInf = -10
      assertEqualsDouble(smile(kMInf), vL * 2.0 / 3.0, 1e-10)
    }
