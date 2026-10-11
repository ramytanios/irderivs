package lib

import lib.dtos.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.*
import lib.syntax.*

import java.time.LocalDate

class HW1FactReprSuite extends munit.FunSuite:

  test("forward swap rate: single-curve fast path matches general path when discount eq reset curve"):

    val t0 = d"2026-10-10"

    val sigma = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.01))

    def swapRateWith(
        discountCurve: YieldCurve[LocalDate],
        resetCurve: YieldCurve[LocalDate]
    ): SwapRate[LocalDate] =
      val floatingRate = new Libor(
        dtos.Currency.EUR,
        Tenor.`3M`,
        0,
        DayCounter.Act360,
        Calendar.all,
        resetCurve,
        ModifiedFollowing
      )
      new SwapRate(
        Tenor.fromYears(5),
        0,
        0,
        Tenor.`1Y`,
        floatingRate,
        DayCounter.Act360,
        Calendar.all,
        ModifiedFollowing,
        dtos.StubConvention.Short,
        dtos.Direction.Backward,
        discountCurve
      )

    // Fast path: discount and reset curves are the SAME instance.
    val sharedCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)
    val fastRate = swapRateWith(sharedCurve, sharedCurve)
    assert(fastRate.discountCurve eq fastRate.floatingRate.resetCurve)

    // General path: numerically identical curves, but DISTINCT instances.
    val discountCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)
    val resetCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)
    val generalRate = swapRateWith(discountCurve, resetCurve)
    assert(!(generalRate.discountCurve eq generalRate.floatingRate.resetCurve))

    val fixingAt = t0 + Tenor.`1Y`
    val (tStart, tEnd) = fastRate.interestPeriod(fixingAt)

    val measure = Measure(fixingAt, dtos.Currency.EUR)

    Rand.choose(-0.1, -0.01, 0.0, 0.01, 0.1).flatMap: lambda =>
      val hw = HW1FactRepr(t0, lambda, sigma, measure)
      val fastFn = hw.forwardSwapRate(fixingAt, tStart, tEnd, fastRate)
      val generalFn = hw.forwardSwapRate(fixingAt, tStart, tEnd, generalRate)
      val vt = hw.variance(fixingAt)
      Rand.normal.map(_ * math.sqrt(vt)).map(x => (fastFn, generalFn, x))
    .view
      .take(10)
      .foreach:
        case (fastFn, generalFn, x) =>
          assertEqualsDouble(fastFn(x), generalFn(x), 1e-10)

  test("forward rate reduces to deterministic forward at t0"):

    val t0 = d"2026-10-10"

    val sigma = PiecewiseConstant(IndexedSeq(0.0), IndexedSeq(0.01))

    val discountCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)

    val fixingAt = t0 + Tenor.`1Y`
    val measure = Measure(fixingAt, dtos.Currency.EUR)

    case class Case(
        lambda: Double,
        rate: Underlying[LocalDate]
    )

    val libor = new Libor(
      dtos.Currency.EUR,
      Tenor.`3M`,
      0,
      DayCounter.Act360,
      Calendar.all,
      discountCurve,
      ModifiedFollowing
    )

    val swapRate = new SwapRate(
      Tenor.fromYears(5),
      0,
      0,
      Tenor.`1Y`,
      new Libor(
        dtos.Currency.EUR,
        Tenor.`3M`,
        0,
        DayCounter.Act360,
        Calendar.all,
        discountCurve,
        ModifiedFollowing
      ),
      DayCounter.Act360,
      Calendar.all,
      ModifiedFollowing,
      dtos.StubConvention.Short,
      dtos.Direction.Backward,
      discountCurve
    )

    val compoundedSwapRate = new CompoundedSwapRate(
      Tenor.fromYears(5),
      0,
      0,
      Tenor.`1Y`,
      new Libor(
        dtos.Currency.EUR,
        Tenor.`1D`,
        0,
        DayCounter.Act360,
        Calendar.all,
        discountCurve,
        ModifiedFollowing
      ),
      Tenor.`3M`,
      DayCounter.Act360,
      Calendar.all,
      ModifiedFollowing,
      dtos.StubConvention.Short,
      dtos.Direction.Backward,
      discountCurve
    )

    val randCase =
      for
        lambda <- Rand.choose(-0.1, -0.01, 0.0, 0.01, 0.1)
        rate <- Rand.choose(libor, swapRate, compoundedSwapRate)
      yield Case(lambda, rate)

    randCase
      .view
      .take(10)
      .foreach:
        case Case(lambda, rate) =>
          val hw = HW1FactRepr(t0, lambda, sigma, measure)
          val hwForward = rate match
            case lr: Libor[LocalDate] =>
              val (ipStart, ipEnd) = lr.interestPeriod(fixingAt)
              hw.forwardLibor(t0, ipStart, ipEnd, lr)(0.0)
            case sr: SwapRate[LocalDate] =>
              val (ipStart, ipEnd) = sr.interestPeriod(fixingAt)
              hw.forwardSwapRate(t0, ipStart, ipEnd, sr)(0.0)
            case csr: CompoundedSwapRate[LocalDate] =>
              val (ipStart, ipEnd) = csr.interestPeriod(fixingAt)
              hw.forwardSwapRate(t0, ipStart, ipEnd, csr)(0.0)
          val deterministicForward = rate.forward(fixingAt)
          assertEqualsDouble(hwForward, deterministicForward, 1e-10)
