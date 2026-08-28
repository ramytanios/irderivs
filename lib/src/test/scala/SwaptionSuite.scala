package lib

import lib.DayCounter.Act360
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class SwaptionSuite extends munit.FunSuite with lib.EitherSyntax:

  test("swaption price"):

    val t = d"2025-11-01"

    val resetCurve = YieldCurve.continuousCompounding(t, 0.02, DayCounter.Act365)
    val discountCurve = resetCurve

    val libor = new Libor(
      dtos.Currency.USD,
      Tenor.`3M`,
      2,
      Act360,
      Calendar.all,
      resetCurve,
      dtos.BusinessDayConvention.ModifiedFollowing
    )

    val swapRate = new SwapRate(
      Tenor.`2Y`,
      0,
      0,
      Tenor.`3M`,
      libor,
      Act360,
      Calendar.all,
      dtos.BusinessDayConvention.ModifiedFollowing,
      dtos.StubConvention.Short,
      dtos.Direction.Backward,
      discountCurve
    )

    val fixingAt = DateLike[LocalDate].plusPeriod(t, Tenor.`1Y`)
    val (startAt, endAt) = libor.interestPeriod(fixingAt)

    val volSurface = VolatilitySurface.fromMoneynessSkew(
      libor.forward,
      Seq(-0.02, -0.01, -0.005, -0.0025, 0.0, 0.0025, 0.005, 0.01, 0.02),
      Seq(100.0, 80.0, 72.0, 70.0, 69.0, 71.0, 74.0, 90.0, 93.0).map(_ / 10000)
    )

    val swaptionPhysical = new Swaption(
      swapRate,
      fixingAt,
      libor.forward(fixingAt) / 2.0,
      dtos.OptionType.Call,
      dtos.Annuity.Physical,
      discountCurve,
      lib.Detachment.default,
      Map.empty
    )

    swaptionPhysical.price(t, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.020045814355300937, 1e-10)

    swaptionPhysical.price(fixingAt, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.0, 1e-10)

    val swaptionCash = new Swaption(
      swapRate,
      fixingAt,
      libor.forward(fixingAt) / 2.0,
      dtos.OptionType.Call,
      dtos.Annuity.Cash,
      discountCurve,
      lib.Detachment.default,
      Map.empty
    )

    swaptionCash.price(endAt, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.0, 1e-10)
