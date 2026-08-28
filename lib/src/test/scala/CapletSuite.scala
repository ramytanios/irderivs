package lib

import lib.DayCounter.Act360
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class CapletSuite extends munit.FunSuite with lib.EitherSyntax:

  test("caplet price"):

    val t = d"2025-10-12"

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

    val fixingAt = DateLike[LocalDate].plusPeriod(t, Tenor.`1Y`)
    val (startAt, endAt) = libor.interestPeriod(fixingAt)

    val volSurface = VolatilitySurface.fromMoneynessSkew(
      libor.forward,
      Seq(-0.02, -0.01, -0.005, -0.0025, 0.0, 0.0025, 0.005, 0.01, 0.02),
      Seq(100.0, 80.0, 72.0, 70.0, 69.0, 71.0, 74.0, 90.0, 93.0).map(_ / 10000)
    )

    val forward = libor.forward(fixingAt)

    val strike = forward / 2.0

    val caplet = new Caplet(
      libor,
      fixingAt,
      startAt,
      endAt,
      endAt,
      dtos.Currency.USD,
      strike,
      discountCurve,
      dtos.OptionType.Call,
      Detachment.default,
      Map.empty
    )

    caplet.price(t, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.0025670027485647476, 1e-10)

    caplet.price(endAt, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.0, 1e-10, s"payoff detached at $endAt")

    val capletWithFixing = new Caplet(
      libor,
      fixingAt,
      startAt,
      endAt,
      endAt,
      dtos.Currency.USD,
      strike,
      discountCurve,
      dtos.OptionType.Call,
      Detachment.default,
      Map(fixingAt -> strike)
    )

    capletWithFixing.price(fixingAt, volSurface).failOrAssert: price =>
      assertEqualsDouble(price, 0.0, 1e-10, s"rate already fixed at $fixingAt")
