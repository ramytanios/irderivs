package lib

import lib.dtos.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.Tenor

import java.time.LocalDate

class CompoundedSwapRateSuite extends munit.FunSuite:

  test("forward calculation"):

    val t0 = d"2025-10-21"

    val resetCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)

    val discountCuve = resetCurve

    val floatingRate = new Libor(
      dtos.Currency.EUR,
      Tenor.`3M`,
      2,
      DayCounter.Act360,
      Calendar.all,
      resetCurve,
      ModifiedFollowing
    )

    val rate = new CompoundedSwapRate(
      Tenor.fromYears(5),
      2,
      0,
      Tenor.`1Y`,
      floatingRate,
      Tenor.`1Y`,
      DayCounter.Act360,
      Calendar.all,
      ModifiedFollowing,
      dtos.StubConvention.Short,
      dtos.Direction.Backward,
      discountCuve
    )

    val fixingAt = Calendar.all.addBusinessPeriod(t0, Tenor.`1Y`)(using ModifiedFollowing)
    assertEqualsDouble(rate.forward(fixingAt), 0.019924721293744743, 1e-12)
