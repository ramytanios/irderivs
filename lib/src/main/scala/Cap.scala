package lib

import syntax.*

case class CapPeriod[T](fixingAt: T, startAt: T, endAt: T, paymentAt: T, fixings: Map[T, Double])

class Cap[T: DateLike](
    val rate: Libor[T],
    val periods: List[CapPeriod[T]],
    val paymentCurrency: dtos.Currency,
    val strike: Double,
    val optionType: dtos.OptionType,
    val discountCurve: YieldCurve[T],
    val detachment: Detachment[T]
):

  def price(t: T, volSurface: VolatilitySurface[T]): Either[Error, Double] =
    periods.traverse: p =>
      new Caplet(
        rate,
        p.fixingAt,
        p.startAt,
        p.endAt,
        p.paymentAt,
        paymentCurrency,
        strike,
        discountCurve,
        optionType,
        detachment,
        p.fixings
      ).price(t, volSurface)
    .map(_.sum)
