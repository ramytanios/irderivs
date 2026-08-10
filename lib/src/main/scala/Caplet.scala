package lib

import lib.quantities.*
import lib.syntax.*

import scala.math.max

class Caplet[T: DateLike](
    val rate: Libor[T],
    val fixingAt: T,
    val startAt: T,
    val endAt: T,
    val paymentAt: T,
    val paymentCurrency: dtos.Currency,
    val strike: Double,
    val discountCurve: YieldCurve[T],
    val optionType: dtos.OptionType,
    val detachment: Detachment[T]
):

  def price(t: T, volSurface: VolatilitySurface[T], fixings: Map[T, Double]): Either[Error, Double] =

    val (_, interestEndAt) = rate.interestPeriod(fixingAt)

    Either
      .raiseWhen(rate.currency != paymentCurrency)(
        Error.Generic("vanilla price does not support quanto")
      )
      .flatMap: _ =>
        Either.raiseWhen((interestEndAt - paymentAt).abs > 7)(
          Error.Generic(s"vanilla pricer does not allow payment convexity")
        )
      .map: _ =>
        if detachment.isDetached(paymentAt, t) then 0.0
        else
          val d = discountCurve.discount(paymentAt)
          val dcf = startAt.yearFractionTo(endAt)(using DateLike[T], rate.dayCounter)
          if t >= fixingAt then
            val rate = fixings(fixingAt)
            d * dcf.value * max(optionType.sign * (rate - strike), 0.0)
          else
            val f = rate.forward(fixingAt)
            val dt = t.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365)
            val vol = volSurface(fixingAt)(strike)
            dcf * bachelier.price(optionType, f, strike, dt.value, vol, d)
