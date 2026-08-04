import lib.*
import lib.dtos
import lib.dtos.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.Rate
import lib.quantities.Tenor

import java.time.LocalDate

class CDFInverterSuite extends munit.FunSuite:

  val t = d"2025-10-13"

  val usd = dtos.Currency.USD

  val resetCurve = YieldCurve.continuousCompounding(t, 0.05, DayCounter.Act365)

  val calendar = Calendar.all

  val libor = new Libor(
    usd,
    Tenor.`1Y`,
    0,
    DayCounter.Act360,
    Calendar.all,
    resetCurve,
    Following
  )

  val forward = libor.forward

  val tExp = DateLike[LocalDate].plusPeriod(t, Tenor.`1Y`)

  val forwardAtExp = forward.apply(tExp)

  val params = CDFInverter.Params(400, 40, 10, 0.001, 0.05)

  val ms = List(
    -4.00, -3.00, -2.00, -1.50, -1.00, -0.50, 0.00, 0.50, 1.00, 1.50, 2.00, 3.00, 4.00
  ).map(_ / 100)
  val ks = ms.map(_ + forwardAtExp)

  test("no arbitrage"):

    val vs0 = List(
      170.145, 152.336, 131.238, 119.941, 108.576, 98.021, 90.377, 88.833, 93.944, 102.947, 113.598,
      136.344, 159.253
    ).map(_ / 10000)

    val skew0 = VolatilitySkew(ks.toIndexedSeq, vs0.toIndexedSeq)
    CDFInverter(t, tExp, skew0, forward, params) match
      case Left(value)  => fail(s"should be arbitrage free, got $value")
      case Right(value) => ()

  test("left asymptotic arbitrage"):

    val vs1 = List(
      210.123, 170.336, 131.238, 119.941, 108.576, 98.021, 90.377, 88.833, 93.944, 102.947, 113.598,
      136.344, 159.253
    ).map(_ / 10000)
    val skew1 = VolatilitySkew(ks.toIndexedSeq, vs1.toIndexedSeq)
    CDFInverter(t, tExp, skew1, forward, params) match
      case Left(Arbitrage.LeftAsymptotic) => ()
      case other                          => fail(s"should have left asymptotic arbitrage, got $other")

  test("density arbitrage"):

    val vs2 =
      List(170.145, 152.336, 131.238, 119.941, 130.00, 150.00, 160.00, 140.00, 93.944, 102.947,
        113.598, 136.344, 159.253
      ).map(_ / 10000)
    val skew2 = VolatilitySkew(ks.toIndexedSeq, vs2.toIndexedSeq)
    CDFInverter(t, tExp, skew2, forward, params) match
      case Left(Arbitrage.Density(_, _)) => ()
      case other                         => fail(s"shoud have density arbitrage, got $other")

  test("right asymptotic arbitrage"):

    val vs3 = List(
      170.145, 152.336, 131.238, 119.941, 108.576, 98.021, 90.377, 88.833, 93.944, 102.947,
      113.598, 170.123, 190.613
    ).map(_ / 10000)
    val skew3 = VolatilitySkew(ks.toIndexedSeq, vs3.toIndexedSeq)
    CDFInverter(t, tExp, skew3, forward, params) match
      case Left(Arbitrage.RightAsymptotic) => ()
      case other                           => fail(s"should have right asymptotic arbitrage, got $other")
