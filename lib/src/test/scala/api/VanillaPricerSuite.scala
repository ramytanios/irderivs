package lib.api

import lib.DateLike
import lib.EitherSyntax
import lib.api.*
import lib.dtos
import lib.literals.*

import java.time.LocalDate

class VanillaPricerSuite extends munit.FunSuite with EitherSyntax:

  test("caplet price"):

    val t = d"2025-10-12"

    val calendarId = dtos.CalendarId("NO_HOLIDAYS")

    val singleCurveId = dtos.CurveId("SINGLE_CURVE")

    val rate: dtos.Underlying = dtos.Underlying.Libor(
      dtos.Currency.USD,
      dtos.Tenor.`3M`,
      2,
      dtos.DayCounter.Act360,
      calendarId,
      dtos.Curve(dtos.Currency.USD, singleCurveId),
      dtos.BusinessDayConvention.ModifiedFollowing
    )

    val volConventions = dtos.VolatilityMarketConventions(
      dtos.Tenor.`10Y`,
      dtos.VolatilityMarketConventions.Libor(
        dtos.Currency.USD,
        2,
        dtos.DayCounter.Act360,
        calendarId,
        dtos.Curve(dtos.Currency.USD, singleCurveId),
        dtos.BusinessDayConvention.ModifiedFollowing
      ),
      dtos.VolatilityMarketConventions.SwapRate(
        2,
        0,
        dtos.Tenor.`3M`,
        dtos.RateId("LIBOR_RATE"),
        dtos.DayCounter.Act360,
        calendarId,
        dtos.BusinessDayConvention.ModifiedFollowing,
        dtos.StubConvention.Short,
        dtos.Direction.Backward,
        dtos.Curve(dtos.Currency.USD, singleCurveId)
      )
    )

    val market = Market(
      tRef = t,
      rates = Map(dtos.RateId("LIBOR_RATE") -> rate),
      curves =
        Map(
          dtos.Curve(dtos.Currency.USD, singleCurveId) ->
            dtos.YieldCurve.ContinuousCompounding(0.02)
        ),
      fixingsByRate = Map.empty,
      volatilities = Map(
        dtos.Currency.USD -> dtos.Volatility.Cube(
          Map(
            dtos.Tenor.`3M` ->
              Map(
                dtos.Tenor.`1Y` ->
                  List(
                    dtos.Moneyness(-0.0200) -> 100.0,
                    dtos.Moneyness(-0.0100) -> 80.0,
                    dtos.Moneyness(-0.0050) -> 72.0,
                    dtos.Moneyness(-0.0025) -> 70.0,
                    dtos.Moneyness(+0.0000) -> 69.0,
                    dtos.Moneyness(+0.0025) -> 71.0,
                    dtos.Moneyness(+0.0050) -> 74.0,
                    dtos.Moneyness(+0.0100) -> 90.0,
                    dtos.Moneyness(+0.0200) -> 93.0
                  ).map((m, v) => m -> v / 10000.0)
              )
          ),
          volConventions
        )
      ),
      calendars = Map(calendarId -> dtos.Calendar[LocalDate](Nil))
    )

    val fixingAt = d"2026-10-12"
    val startAt = d"2026-10-14"
    val endAt = d"2027-01-14"

    val caplet = dtos.Payoff.Caplet(
      dtos.RateId("LIBOR_RATE"),
      fixingAt,
      startAt,
      endAt,
      endAt,
      dtos.Currency.USD,
      0.009887915724457295,
      dtos.Curve(dtos.Currency.USD, singleCurveId),
      dtos.OptionType.Call
    )

    new Api(market).price(caplet).failOrAssert: price =>
      assertEqualsDouble(price, 0.0025670027485647476, 1e-10)
