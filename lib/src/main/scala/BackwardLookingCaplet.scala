package lib

import lib.quantities.*
import lib.syntax.*

import scala.math.max
import scala.math.pow

class BackwardLookingCaplet[T: DateLike](
    val startAt: T,
    val endAt: T,
    val rate: Libor[T],
    val paymentCurrency: dtos.Currency,
    val paymentAt: T,
    val strike: Double,
    val optionType: dtos.OptionType,
    val discountCurve: YieldCurve[T],
    val stub: dtos.StubConvention,
    val direction: dtos.Direction,
    val detachment: Detachment[T],
    val fixings: Map[T, Double]
):
  private val noHolidaysCal = Calendar.all

  def price(t: T, cube: VolatilityCube[T]): Either[Error, Double] =

    val discount = discountCurve.discount(paymentAt)

    def syntheticFutLibor(from: T, to: T): Libor[T] =
      val n = noHolidaysCal.countBusinessDays(from, to)
      val tenor = Tenor.fromDays(n.toInt)
      new Libor[T](
        rate.currency,
        tenor,
        rate.spotLag,
        rate.dayCounter,
        noHolidaysCal,
        rate.resetCurve,
        rate.bdConvention
      )

    def futPrice(futRate: CompoundedRate[T], futStrike: Double): Either[Error, Double] =
      given DayCounter = DayCounter.Act365
      val schedule = futRate.schedule
      futRate.forward(t).map: forward =>
        val futLibor = syntheticFutLibor(futRate.from, futRate.to)
        val vol = cube(futLibor.tenor)(futRate.firstFixingAt)(futStrike)
        val dt = t.yearFractionTo(futRate.firstFixingAt) +
          schedule.indices.init.foldMap(i =>
            schedule(i).fixingAt.yearFractionTo(schedule(i + 1).fixingAt) *
              pow(schedule(i + 1).startAt.yearFractionTo(futRate.to).value, 2)
          ) / pow(futRate.from.yearFractionTo(futRate.to).value, 2)
        bachelier.price(optionType, forward, futStrike, vol, dt, discount)

    Either
      .raiseWhen(rate.currency != paymentCurrency)(
        Error.Generic("vanilla price does not support quanto")
      )
      .flatMap: _ =>
        Either.raiseWhen((endAt - paymentAt).abs > 7)(
          Error.Generic(s"vanilla pricer does not allow payment convexity")
        )
          .flatMap: _ =>
            if detachment.isDetached(paymentAt, t) then Right(0.0)
            else
              val fullRate = CompoundedRate[T](startAt, endAt, rate, stub, direction, fixings)
              val fullDcf = fullRate.dcf
              if t < fullRate.firstFixingAt then
                futPrice(fullRate, strike).map(fullDcf * _)
              else if t >= fullRate.lastFixingAt then
                val fullRateValue = (fullRate.fullCompoundingFactor() - 1) / fullDcf.value
                Right:
                  fullDcf * discount * max(optionType.sign * (fullRateValue - strike), 0.0)
              else
                val obsIdx = fullRate.findObservationIdx(t)
                val futIdx = obsIdx + 1
                val schedule = fullRate.schedule
                val cf = fullRate.compoundingFactor(schedule(obsIdx).fixingAt)
                val futRate =
                  new CompoundedRate[T](rate, schedule.slice(futIdx, schedule.length), fixings)
                val futDcf = futRate.dcf.value
                val futStrike = ((fullDcf * strike + 1) / cf - 1) / futDcf
                futPrice(futRate, futStrike).map(futDcf * cf * _)
