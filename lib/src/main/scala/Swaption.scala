package lib

import lib.quantities.*
import lib.syntax.*

import scala.math.max

class Swaption[T: DateLike](
    val rate: SwapLike[T],
    val fixingAt: T,
    val strike: Double,
    val optionType: dtos.OptionType,
    val annuity: dtos.Annuity,
    val discountCurve: YieldCurve[T],
    val detachment: Detachment[T],
    val fixings: Map[T, Double]
):

  given DayCounter = rate.fixedDayCounter

  def price(t: T, volSurface: VolatilitySurface[T]): Either[Error, Double] =
    val fwd = rate.forward(fixingAt)
    val vol = volSurface(fixingAt)(strike)
    val (swapStartAt, swapEndAt) = rate.interestPeriod(fixingAt)
    val fixed = rate.fixedSchedule(swapStartAt, swapEndAt)
    val dt = t.yearFractionTo(fixingAt)(using DateLike[T], DayCounter.Act365).value
    annuity match
      case dtos.Annuity.Physical =>
        // enter into a real swap contract
        if t >= fixingAt then Right(0.0)
        else
          val discount =
            val a = fixed.foldLeft(0.0):
              case (acc, FixedCoupon(startAt, endAt, paymentAt)) =>
                acc + rate.discountCurve.discount(paymentAt) * startAt.yearFractionTo(endAt).value
            val adj = discountCurve.discount(fixingAt) / rate.discountCurve.discount(fixingAt)
            a * adj
          bachelier.price(optionType, fwd, strike, dt, vol, discount).asRight[Error]
      case dtos.Annuity.Cash =>
        // replace physical annuity with an approx. cash payment at swap start date
        val paymentAt = swapStartAt
        def cashAnnuity(v: Double): Double =
          fixed.indices.reverse.foldLeft(0.0)((cash, i) =>
            val dcf = fixed(i).from.yearFractionTo(fixed(i).to).value
            val discount = 1.0 / (1.0 + v * dcf)
            (dcf + cash) * discount
          )
        if detachment.isDetached(paymentAt, t) then 0.0.asRight[Error]
        else
          val discount = discountCurve.discount(paymentAt)
          if t >= fixingAt then
            val value = fixings.getOrElse(t, rate.forward(t))
            val cash = cashAnnuity(value)
            (discount * cash * max(optionType.sign * (value - strike), 0.0)).asRight[Error]
          else
            val cash = cashAnnuity(fwd)
            bachelier.price(optionType, fwd, strike, dt, vol, cash * discount).asRight[Error]
