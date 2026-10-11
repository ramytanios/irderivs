package lib

import cats.syntax.all.*

import scala.math.*

class HW1[T: DateLike](
    val t0: T,
    val lambda: Double,
    val sigma: PiecewiseConstant
):

  private[lib] def tvCaplet(
      caplet: Caplet[T],
      vAtExpiry: Double,
      factRepr: HW1FactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = caplet.rate
    val (tStart, tEnd, tPay) = (caplet.startAt, caplet.endAt, caplet.paymentAt)
    val dcf = DateLike[T].yearFraction(tStart, tEnd)(using rate.dayCounter)
    val discTp = caplet.discountCurve.discount(tPay)
    val totalVar = pow(factRepr.B(tStart, tEnd), 2) * vAtExpiry
    val forward = 1 / factRepr.A(tStart, tEnd, vAtExpiry, rate.resetCurve) * exp(0.5 * totalVar)
    val strike = 1 + dcf * caplet.strike

    Either.catchNonFatal(black.price(caplet.optionType, forward, strike, totalVar, discTp))
      .leftMap(th => lib.Error(th.getMessage))

  def price(caplet: Caplet[T]): Either[lib.Error, Double] =
    val measure = Measure(caplet.paymentAt, caplet.paymentCurrency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    tvCaplet(caplet, factRepr.variance(caplet.fixingAt), factRepr)

  // E^T{a * e^{b * x_T} * I{x_T>=xStar}} where x_T ~ N(0, V_T)
  private inline def E(p: ExpAffine, xStar: Double, vT: Double): Double =
    val a = p.a
    val b = p.b
    val sT = sqrt(vT)
    a * exp(0.5 * vT * b * b) * normal.cdf(b * sT - xStar / sT)

  private[lib] def tvSwaption(
      swaption: Swaption[T],
      vAtExpiry: Double,
      factRepr: HW1FactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = swaption.rate
    val tExpiry = swaption.fixingAt
    val (sipStart, sipEnd) = rate.interestPeriod(tExpiry)
    val fn = factRepr.swapRate(sipStart, sipEnd, rate)
    RootSolver.brent(fn, -10, 10).map: xStar =>
      val discFixing = swaption.discountCurve.discount(tExpiry)
      val valueFixed = rate.fixedSchedule(sipStart, sipEnd).foldLeft(0.0):
        case (acc, FixedCoupon[T](tStart, tEnd, tPay)) =>
          val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using rate.fixedDayCounter)
          val p = factRepr.zeroCoupon(tExpiry, tPay, swaption.discountCurve)
          acc + dcfStartEnd * E(p, xStar, vAtExpiry)
      val isDiscSameReset = rate.discountCurve eq rate.floatingRate.resetCurve
      val valueFloating = if isDiscSameReset then
        val curve = rate.discountCurve
        val pStart = factRepr.zeroCoupon(tExpiry, sipStart, curve)
        val pEnd = factRepr.zeroCoupon(tExpiry, sipEnd, curve)
        E(pStart, xStar, vAtExpiry) - E(pEnd, xStar, vAtExpiry)
      else
        val ip = rate match
          case sr: SwapRate[T]            => (fc: FloatingCoupon[T]) => rate.interestPeriod(fc.fixingAt)
          case csr: CompoundedSwapRate[T] => (fc: FloatingCoupon[T]) => fc.from -> fc.to
        rate.floatingSchedule(sipStart, sipEnd).foldLeft(0.0):
          case (acc, fc @ FloatingCoupon[T](tFixing, tStart, tEnd, tPay)) =>
            val floatingDayCounter = rate.floatingRate.dayCounter
            val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using floatingDayCounter)
            val (ipStart, ipEnd) = ip(fc)
            val dcfIp = DateLike[T].yearFraction(ipStart, ipEnd)(using floatingDayCounter)
            val pDiscTPay = factRepr.zeroCoupon(tExpiry, tPay, swaption.discountCurve)
            val pResTStart = factRepr.zeroCoupon(tExpiry, tStart, rate.floatingRate.resetCurve)
            val pResTEnd = factRepr.zeroCoupon(tExpiry, tEnd, rate.floatingRate.resetCurve)
            acc +
              dcfStartEnd / dcfIp * (
                E(pDiscTPay.mul(pResTStart).div(pResTEnd), xStar, vAtExpiry) -
                  E(pDiscTPay, xStar, vAtExpiry)
              )
      discFixing * (valueFloating - swaption.strike * valueFixed)

  def price(swaption: Swaption[T]): Either[lib.Error, Double] =
    val measure = Measure(swaption.fixingAt, swaption.rate.currency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    tvSwaption(swaption, factRepr.variance(swaption.fixingAt), factRepr)
