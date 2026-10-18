package lib

import lib.dtos.Annuity
import lib.dtos.BusinessDayConvention
import lib.dtos.Currency
import lib.dtos.Direction
import lib.dtos.OptionType
import lib.dtos.StubConvention
import lib.quantities.*
import lib.syntax.*

import java.time.LocalDate

object rand:

  private val randLiborTenor = Rand.choose(Tenor.`3M`, Tenor.`6M`)
  private val randSwapTenor = Rand.choose(Tenor.`1Y`, Tenor.`2Y`, Tenor.`10Y`)
  private val randPaymentDelay = Rand.choose(0, 1, 2)
  private val randSpotLag = Rand.choose(0, 1, 2)
  private val randDayCounter = Rand.choose(DayCounter.Act360, DayCounter.Act365)
  private val randBdConvention = Rand.choose(BusinessDayConvention.values*)
  private val randStub = Rand.choose(StubConvention.values*)
  private val randDirection = Rand.choose(Direction.values*)

  def randHWLambda: Rand[Double] = Rand.between(0.01, 0.3)

  def randLibor(
      t0: LocalDate,
      useZeroSpotLag: Boolean = false
  ): Rand[Libor[LocalDate]] =
    for
      ccy <- Rand.choose(Currency.values*)
      tenor <- randLiborTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      resetCurve <- Rand.between(0.0, 5.0).map(r => YieldCurve.continuousCompounding(t0, r / 100.0))
    yield new Libor(ccy, tenor, spotLag, dayCounter, Calendar.all, resetCurve, bdConvention)

  def randDaily(t0: LocalDate): Rand[Libor[LocalDate]] =
    for
      ccy <- Rand.choose(Currency.values*)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      resetCurve <- Rand.between(0.0, 5.0).map(r => YieldCurve.continuousCompounding(t0, r / 100.0))
    yield new Libor(ccy, Tenor.`1D`, 0, dayCounter, Calendar.all, resetCurve, bdConvention)

  def randSwapRate(
      t0: LocalDate,
      useZeroSpotLag: Boolean = false,
      useZeroPayDelay: Boolean = false
  ): Rand[SwapRate[LocalDate]] =
    for
      floatingRate <- randLibor(t0, useZeroSpotLag)
      tenor <- randSwapTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      paymentDelay <- Rand.cond(useZeroPayDelay, 0, randPaymentDelay)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      stub <- randStub
      direction <- randDirection
      discountCurve <-
        Rand.between(0.0, 5.0).map(r => YieldCurve.continuousCompounding(t0, r / 100.0))
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
      t0: LocalDate,
      useZeroSpotLag: Boolean = false,
      useZeroPayDelay: Boolean = false
  ): Rand[CompoundedSwapRate[LocalDate]] =
    for
      floatingRate <- randDaily(t0)
      tenor <- randSwapTenor
      spotLag <- Rand.cond(useZeroSpotLag, 0, randSpotLag)
      paymentDelay <- Rand.cond(useZeroPayDelay, 0, randPaymentDelay)
      dayCounter <- randDayCounter
      bdConvention <- randBdConvention
      stub <- randStub
      direction <- randDirection
      fixedPeriod <- Rand.choose(Tenor.`3M`, Tenor.`6M`)
      floatingPeriod <- Rand.choose(Tenor.`3M`, Tenor.`6M`)
      discountCurve <-
        Rand.between(0.0, 5.0).map(r => YieldCurve.continuousCompounding(t0, r / 100.0))
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

  def randCaplet(t0: LocalDate, useZeroPayDelay: Boolean = false): Rand[Caplet[LocalDate]] =
    for
      rate <- rand.randLibor(t0, useZeroPayDelay)
      tFixing <- Rand.choose(Tenor.`3M`, Tenor.`3M`, Tenor.`1Y`, Tenor.`2Y`).map(t0 + _)
      (tStart, tEnd) = rate.interestPeriod(tFixing)
      tPay <- if useZeroPayDelay then tEnd.pure[Rand]
      else Rand.choose(0, 1, 2, 3).map(d => tEnd + Tenor(d, Tenor.Unit.Day))
      strike <- Rand.choose(-1, 1).map(pct => rate.forward(tFixing) * (1 + pct / 100.0))
      kind <- Rand.choose(OptionType.values*)
      discountCurve <-
        Rand.between(0.0, 5.0).map(d => YieldCurve.continuousCompounding(t0, d / 100.0))
    yield lib.Caplet(
      rate,
      tFixing,
      tStart,
      tEnd,
      tPay,
      rate.currency,
      strike,
      discountCurve,
      kind,
      Detachment.default,
      Map.empty
    )

  def randSwaption(
      t0: LocalDate,
      useZeroPayDelay: Boolean = false
  ): Rand[Swaption[LocalDate]] =
    for
      rate <- (
        rand.randSwapRate(t0, useZeroPayDelay = useZeroPayDelay),
        rand.randCompoundedSwapRate(t0, useZeroPayDelay = useZeroPayDelay)
      ).tupled.flatMap((sr, csr) => Rand.choose(sr, csr))
      tFixing <- Rand.choose(Tenor.`2Y`, Tenor.`10Y`, Tenor.`20Y`).map(t0 + _)
      (tStart, tEnd) = rate.interestPeriod(tFixing)
      strike <- Rand.choose(-1, 1).map(pct => rate.forward(tFixing) * (1 + pct / 100.0))
      kind <- Rand.choose(OptionType.values*)
      discountCurve <-
        Rand.between(0.0, 5.0).map(d => YieldCurve.continuousCompounding(t0, d / 100.0))
      annuity <- Rand.choose(Annuity.values*)
    yield lib.Swaption(
      rate,
      tFixing,
      strike,
      kind,
      annuity,
      discountCurve,
      Detachment.default,
      Map.empty
    )
