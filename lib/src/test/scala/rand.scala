package lib

import lib.dtos.BusinessDayConvention
import lib.dtos.Currency
import lib.quantities.*
import lib.syntax.*

import java.time.LocalDate
import lib.dtos.StubConvention
import lib.dtos.Direction

object rand:

  private val randLiborTenor = Rand.choose(Tenor.`3M`, Tenor.`6M`)
  private val randSwapTenor = Rand.choose(Tenor.`1Y`, Tenor.`2Y`, Tenor.`10Y`)
  private val randPaymentDelay = Rand.choose(0, 1, 2)
  private val randSpotLag = Rand.choose(0, 1, 2)
  private val randDayCounter = Rand.choose(DayCounter.Act360, DayCounter.Act365)
  private val randBdConvention = Rand.choose(BusinessDayConvention.values*)
  private val randStub = Rand.choose(StubConvention.values*)
  private val randDirection = Rand.choose(Direction.values*)

  def randLibor(
      resetCurve: YieldCurve[LocalDate],
      useZeroSpotLag: Boolean = false
  ): Rand[Libor[LocalDate]] =
    for
      ccy <- Rand.choose(Currency.values*)
      tenor <- randLiborTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
    yield new Libor(ccy, tenor, spotLag, dayCounter, Calendar.all, resetCurve, bdConvention)

  def randDaily(resetCurve: YieldCurve[LocalDate]): Rand[Libor[LocalDate]] =
    for
      ccy <- Rand.choose(Currency.values*)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
    yield new Libor(ccy, Tenor.`1D`, 0, dayCounter, Calendar.all, resetCurve, bdConvention)

  def randSwapRate(
      resetCurve: YieldCurve[LocalDate],
      discountCurve: YieldCurve[LocalDate],
      useZeroSpotLag: Boolean = false,
      useZeroPayDelay: Boolean = false
  ): Rand[SwapRate[LocalDate]] =
    for
      floatingRate <- randLibor(resetCurve, useZeroSpotLag)
      tenor <- randSwapTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      paymentDelay <- Rand.cond(useZeroPayDelay, 0, randPaymentDelay)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      stub <- randStub
      direction <- randDirection
    yield new SwapRate(
      tenor,
      spotLag,
      paymentDelay,
      floatingRate.tenor,
      floatingRate,
      dayCounter,
      Calendar.all,
      bdConvention,
      stub,
      direction,
      discountCurve
    )

  def randCompoundedSwapRate(
      resetCurve: YieldCurve[LocalDate],
      discountCurve: YieldCurve[LocalDate],
      useZeroSpotLag: Boolean = false,
      useZeroPayDelay: Boolean = false
  ): Rand[CompoundedSwapRate[LocalDate]] =
    for
      floatingRate <- randDaily(resetCurve)
      tenor <- randSwapTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      paymentDelay <- Rand.cond(useZeroPayDelay, 0, randPaymentDelay)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      stub <- randStub
      direction <- randDirection
      fixedPeriod <- Rand.choose(Tenor.`3M`, Tenor.`6M`)
      floatingPeriod <- Rand.choose(Tenor.`3M`, Tenor.`6M`)
    yield new CompoundedSwapRate(
      tenor,
      spotLag,
      paymentDelay,
      fixedPeriod,
      floatingRate,
      floatingPeriod,
      dayCounter,
      Calendar.all,
      bdConvention,
      stub,
      direction,
      discountCurve
    )
