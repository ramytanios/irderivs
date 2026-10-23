package lib

import lib.dtos.Currency
import lib.quantities.YearFraction
import lib.syntax.*

class HW1HW1Black[T: DateLike](
    val t0: T,
    val ccyDomestic: Currency,
    val ccyForeign: Currency,
    val hwDomestic: (Double, PiecewiseConstant),
    val hwForeign: (Double, PiecewiseConstant),
    val blackFXVol: PiecewiseConstant,
    val corrDomesticFX: PiecewiseConstant,
    val corrForeignFX: PiecewiseConstant,
    val corrDomesticForeign: PiecewiseConstant
):

  def price(caplet: Caplet[T]): Either[lib.Error, Double] =
    val rate = caplet.rate
    val (tStart, tEnd, tPay) = (caplet.startAt, caplet.endAt, caplet.paymentAt)
    val domCurrency = caplet.paymentCurrency
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.dayCounter)
    val discPay = caplet.discountCurve.discount(tPay)
    val factRepr = new HW1HW1BlackFactRepr[T](
      t0,
      ccyDomestic,
      ccyForeign,
      hwDomestic,
      hwForeign,
      blackFXVol,
      corrDomesticFX,
      corrForeignFX,
      corrDomesticForeign,
      Measure(tPay, domCurrency)
    )
    val forward = factRepr.logNormalForward(rate, caplet.fixingAt)
    val strike = 1 + dcf * caplet.strike
    val b = factRepr.factReprFor.B(tStart, tEnd)
    val variance = b * b * factRepr.varianceForeign(caplet.fixingAt)
    Either.catchNonFatal(
      black.price(caplet.optionType, forward, strike, variance, discPay)
    ).leftMap(th => lib.Error(th.getMessage))
