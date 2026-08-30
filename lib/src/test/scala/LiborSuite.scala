package lib

import lib.dtos.BusinessDayConvention.*
import lib.literals.*
import lib.quantities.Rate
import lib.quantities.Tenor

import java.time.LocalDate

class LiborSuite extends munit.FunSuite:

  test("forward calculation"):

    val t0 = d"2025-10-12"

    val resetCurve = YieldCurve.continuousCompounding(t0, 0.02, DayCounter.Act365)

    val libor = new Libor(
      dtos.Currency.USD,
      Tenor.`1D`,
      0,
      DayCounter.Act360,
      Calendar.all,
      resetCurve,
      ModifiedFollowing
    )

    val fixingAt = Calendar.all.addBusinessPeriod(t0, Tenor.`1Y`)(using ModifiedFollowing)
    assertEqualsDouble(libor.forward(fixingAt), 0.019726567846252152, 1e-12)
