package lib.api

import cats.syntax.all.*
import lib.dtos
import lib.quantities.*
import lib.quantities.Tenor.toYearFraction

import scala.math.Ordering.Implicits.*

class Lib[T: lib.DateLike](market: Market[T]):

  def buildYieldCurve(curve: dtos.Curve): Either[lib.Error, lib.YieldCurve[T]] =
    market.yieldCurve(curve).map:
      case dtos.YieldCurve.Discounts(discounts) =>
        lib.YieldCurve(market.t, discounts.toIndexedSeq)
      case dtos.YieldCurve.ContinuousCompounding(rate) =>
        lib.YieldCurve.continuousCompounding(market.t, rate)

  def buildCalendar(calendar: dtos.Calendar[T]): Either[lib.Error, lib.Calendar[T]] =
    Either.catchNonFatal(lib.Calendar.fromHolidays(calendar.holidays.toIndexedSeq)).leftMap: th =>
      lib.Error.Generic(s"unable to build calendar: ${th.getMessage}")

  def buildFixings(rate: dtos.RateId): Either[lib.Error, Map[T, Double]] =
    market.fixings(rate).orElse(Seq.empty.asRight).flatMap: fixings =>
      fixings.groupBy(_.t).toSeq.traverse: (fixingAt, all) =>
        all.headOption.toRight(MarketError.MissingFixingAt(
          rate,
          fixingAt
        )).map(_.value).tupleLeft(fixingAt)
      .map(_.toMap)

  def buildVolSurface(
      currency: dtos.Currency,
      tenor: lib.quantities.Tenor
  ): Either[lib.Error, lib.VolatilitySurface[T]] =
    market.volCube(currency).flatMap:
      case dtos.Volatility.Cube(cube, conventions) =>
        market.volSurface(currency, tenor).flatMap: surface =>
          buildVolConventions(conventions, tenor).map: rate =>
            val skews = surface.toList.map:
              case (expTenor, skew) =>
                val expiry =
                  rate.calendar.addBusinessPeriod(market.t, expTenor)(using rate.bdConvention)
                expiry -> lib.Lazy:
                  val (ms, vs0) = skew.unzip
                  val fwd = rate.forward(expiry)
                  val ks = ms.map(fwd + _.value)
                  lib.VolatilitySkew(ks.toIndexedSeq, vs0.toIndexedSeq)
            val sortedSkews = skews.sortBy(_(0))(using lib.syntax.given_Ordering_T)
            lib.VolatilitySurface[T](market.t, rate.forward, sortedSkews.toIndexedSeq)
      case dtos.Volatility.Flat(vol) =>
        lib.VolatilitySurface.flat[T](vol).asRight[lib.Error]

  def buildVolCube(currency: dtos.Currency): Either[lib.Error, lib.VolatilityCube[T]] =
    market.volCube(currency).flatMap:
      case dtos.Volatility.Cube(cube, conventions) =>
        val surfaces = cube.toList.traverse: (tenor, _) =>
          buildVolSurface(currency, tenor).tupleLeft(tenor)
        .map(_.toIndexedSeq)
        val forwards = cube.keys.map(t => (t: Tenor)).toList.traverse: tenor =>
          buildVolConventions(conventions, tenor).map(_.forward).tupleLeft(tenor)
        .map(_.toMap)
        (surfaces, forwards).tupled.map: (surfaces, forwards) =>
          val sortedSurfaces = surfaces.sortBy((t, _) => t.toYearFraction.value)
            .map((t, e) => (t: Tenor) -> e)
          lib.VolatilityCube[T](sortedSurfaces, forwards)
      case dtos.Volatility.Flat(vol) =>
        lib.VolatilityCube.flat[T](vol).asRight[lib.Error]

  def buildVolConventions(volConventions: dtos.VolatilityMarketConventions, tenor: Tenor) =
    if (volConventions.boundaryTenor: Tenor) >= tenor then
      toLibor(volConventions.liborRate, tenor)
    else toSwapRate(volConventions.swapRate, tenor)

  def buildVolConventions(
      currency: dtos.Currency,
      tenor: Tenor
  ): Either[lib.Error, lib.Underlying[T]] =
    market.volCube(currency).flatMap:
      case dtos.Volatility.Cube(_, conventions) => buildVolConventions(conventions, tenor)
      case dtos.Volatility.Flat(_) =>
        Left(lib.Error.Generic(s"vol is flat, no convetions available"))

  private def toLibor(libor: dtos.VolatilityMarketConventions.Libor, tenor: Tenor) =
    buildYieldCurve(libor.resetCurve).flatMap: resetCurve =>
      market.calendar(libor.calendar).flatMap: calendar =>
        buildCalendar(calendar).map: calendar =>
          new lib.Libor[T](
            libor.currency,
            tenor,
            libor.spotLag,
            toDayCounter(libor.dayCounter),
            calendar,
            resetCurve,
            libor.bdConvention
          )

  private def toSwapRate(swapRate: dtos.VolatilityMarketConventions.SwapRate, tenor: Tenor) =
    buildYieldCurve(swapRate.discountCurve).flatMap: discountCurve =>
      buildLibor(swapRate.floatingRate).flatMap: liborRate =>
        market.calendar(swapRate.calendar).flatMap: calendar =>
          buildCalendar(calendar).map: calendar =>
            new lib.SwapRate[T](
              tenor,
              swapRate.spotLag,
              swapRate.paymentDelay,
              swapRate.fixedPeriod,
              liborRate,
              toDayCounter(swapRate.fixedDayCounter),
              calendar,
              swapRate.bdConvention,
              swapRate.stub,
              swapRate.direction,
              discountCurve
            )

  private def toLibor(libor: dtos.Underlying.Libor) =
    buildYieldCurve(libor.resetCurve).flatMap: resetCurve =>
      market.calendar(libor.calendar).flatMap: calendar =>
        buildCalendar(calendar).map: calendar =>
          new lib.Libor[T](
            libor.currency,
            libor.tenor,
            libor.spotLag,
            toDayCounter(libor.dayCounter),
            calendar,
            resetCurve,
            libor.bdConvention
          )

  private def toSwapRate(swapRate: dtos.Underlying.SwapRate) =
    buildYieldCurve(swapRate.discountCurve).flatMap: discountCurve =>
      buildLibor(swapRate.floatingRate).flatMap: liborRate =>
        market.calendar(swapRate.calendar).flatMap: calendar =>
          buildCalendar(calendar).map: calendar =>
            new lib.SwapRate[T](
              swapRate.tenor,
              swapRate.spotLag,
              swapRate.paymentDelay,
              swapRate.fixedPeriod,
              liborRate,
              toDayCounter(swapRate.fixedDayCounter),
              calendar,
              swapRate.bdConvention,
              swapRate.stub,
              swapRate.direction,
              discountCurve
            )

  private def toCompoundedSwapRate(swapRate: dtos.Underlying.CompoundedSwapRate) =
    buildYieldCurve(swapRate.discountCurve).flatMap: discountCurve =>
      buildLibor(swapRate.floatingRate).flatMap: liborRate =>
        market.calendar(swapRate.calendar).flatMap: calendar =>
          buildCalendar(calendar).map: calendar =>
            new lib.CompoundedSwapRate[T](
              swapRate.tenor,
              swapRate.spotLag,
              swapRate.paymentDelay,
              swapRate.fixedPeriod,
              liborRate,
              swapRate.fixedPeriod,
              toDayCounter(swapRate.fixedDayCounter),
              calendar,
              swapRate.bdConvention,
              swapRate.stub,
              swapRate.direction,
              discountCurve
            )

  def buildLibor(rate: dtos.RateId): Either[lib.Error, lib.Libor[T]] =
    market.rate(rate).flatMap:
      case libor: dtos.Underlying.Libor => toLibor(libor)
      case _                            => lib.Error.Generic(s"$rate is not a libor").asLeft

  def buildSwapRate(rate: dtos.RateId): Either[lib.Error, lib.SwapRate[T]] =
    market.rate(rate).flatMap:
      case swapRate: dtos.Underlying.SwapRate =>
        toSwapRate(swapRate)
      case _ => lib.Error.Generic(s"$rate is not a swap rate").asLeft

  def buildCompoundedSwapRate(rate: dtos.RateId): Either[lib.Error, lib.CompoundedSwapRate[T]] =
    market.rate(rate).flatMap:
      case swapRate: dtos.Underlying.CompoundedSwapRate =>
        toCompoundedSwapRate(swapRate)
      case _ => lib.Error.Generic(s"$rate is not a compounded swap rate").asLeft

  def buildCaplet(caplet: dtos.Payoff.Caplet[T]): Either[lib.Error, lib.Caplet[T]] =
    buildLibor(caplet.rate).flatMap: libor =>
      buildYieldCurve(caplet.discountCurve).map: discountCurve =>
        new lib.Caplet[T](
          libor,
          caplet.fixingAt,
          caplet.startAt,
          caplet.endAt,
          caplet.paymentAt,
          caplet.paymentCurrency,
          caplet.strike,
          discountCurve,
          caplet.optionType,
          lib.Detachment.default[T]
        )

  def buildSwaption(swaption: dtos.Payoff.Swaption[T]): Either[lib.Error, lib.Swaption[T]] =
    buildSwapRate(swaption.rate)
      .orElse(buildCompoundedSwapRate(swaption.rate))
      .flatMap: rate =>
        buildYieldCurve(swaption.discountCurve).map: discountCurve =>
          new lib.Swaption[T](
            rate,
            swaption.fixingAt,
            swaption.strike,
            swaption.optionType,
            swaption.annuity,
            discountCurve,
            lib.Detachment.default[T]
          )

  def buildBackwardLookingCaplet(caplet: dtos.Payoff.BackwardLookingCaplet[T])
      : Either[lib.Error, lib.BackwardLookingCaplet[T]] =
    buildLibor(caplet.rate).flatMap: libor =>
      buildYieldCurve(caplet.discountCurve).map: discountCurve =>
        new lib.BackwardLookingCaplet[T](
          caplet.startAt,
          caplet.endAt,
          libor,
          caplet.paymentCurrency,
          caplet.paymentAt,
          caplet.strike,
          caplet.optionType,
          discountCurve,
          caplet.stub,
          caplet.direction,
          lib.Detachment.default[T]
        )
