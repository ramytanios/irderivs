package lib

import cats.syntax.all.*

import scala.math.*

class HW1[T: DateLike](
    val t0: T,
    val lambda: Double,
    val sigma: PiecewiseConstant
):

  // log-normal forward E_t^{T_p} { 1 + dcf(Ts, Te) * L(t, Ts, Te) }
  private[lib] inline def LNF(
      rate: Libor[T],
      tFixing: T,
      vFixing: Double,
      factRepr: HW1FactRepr[T]
  ): (Double, Double) =
    val (tStart, tEnd) = rate.interestPeriod(tFixing)
    val totalVar = pow(factRepr.B(tStart, tEnd), 2) * vFixing
    totalVar -> 1 / factRepr.A(tStart, tEnd, vFixing, rate.resetCurve) * exp(0.5 * totalVar)

  private[lib] def tvCaplet(
      caplet: Caplet[T],
      vAtExpiry: Double,
      factRepr: HW1FactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = caplet.rate
    val dcf = DateLike[T].yearFraction(caplet.startAt, caplet.endAt)(using rate.dayCounter)
    val discTp = caplet.discountCurve.discount(caplet.paymentAt)
    val (totalVar, forward) = LNF(rate, caplet.fixingAt, vAtExpiry, factRepr)
    val strike = 1 + dcf * caplet.strike

    Either.catchNonFatal(
      black.price(caplet.optionType, forward, strike, totalVar, discTp)
    ).leftMap(th => lib.Error(th.getMessage))

  def price(caplet: Caplet[T]): Either[lib.Error, Double] =
    val measure = Measure(caplet.paymentAt, caplet.paymentCurrency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    tvCaplet(caplet, factRepr.variance(caplet.fixingAt), factRepr)

  // E^T{ a * e^{b * x_T} * I{sign * (x_T - xStar) >= 0} } where x_T ~ N(0, V_T)
  private inline def E(p: ExpAffine, xStar: Double, vT: Double, sign: Int): Double =
    val a = p.a
    val b = p.b
    val sT = sqrt(vT)
    a * exp(0.5 * vT * b * b) * normal.cdf(sign * (b * sT - xStar / sT))

  private[lib] def tvSwaption(
      swaption: Swaption[T],
      vAtExpiry: Double,
      factRepr: HW1FactRepr[T]
  ): Either[lib.Error, Double] =
    val rate = swaption.rate
    val tExpiry = swaption.fixingAt
    val sign = swaption.optionType.sign
    val exSign = -sign
    val (sipStart, sipEnd) = rate.interestPeriod(tExpiry)
    val fn = factRepr.forwardSwapRate(tExpiry, sipStart, sipEnd, rate)
    RootSolver.brent(x => fn(x) - swaption.strike, -10, 10).map: xStar =>
      val discFixing = swaption.discountCurve.discount(tExpiry)
      val valueFixed = rate.fixedSchedule(sipStart, sipEnd).foldLeft(0.0):
        case (acc, FixedCoupon[T](tStart, tEnd, tPay)) =>
          val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using rate.fixedDayCounter)
          val p = factRepr.forwardZeroCoupon(tExpiry, tPay, rate.discountCurve, vAtExpiry)
          acc + dcfStartEnd * E(p, xStar, vAtExpiry, exSign)
      val isDiscSameReset = rate.discountCurve eq rate.floatingRate.resetCurve
      val valueFloating = if isDiscSameReset then
        val curve = rate.discountCurve
        val pStart = factRepr.forwardZeroCoupon(tExpiry, sipStart, curve, vAtExpiry)
        val pEnd = factRepr.forwardZeroCoupon(tExpiry, sipEnd, curve, vAtExpiry)
        E(pStart, xStar, vAtExpiry, exSign) - E(pEnd, xStar, vAtExpiry, exSign)
      else
        val ip = rate match
          case sr: SwapRate[T] =>
            (fc: FloatingCoupon[T]) => rate.floatingRate.interestPeriod(fc.fixingAt)
          case csr: CompoundedSwapRate[T] => (fc: FloatingCoupon[T]) => fc.from -> fc.to
        rate.floatingSchedule(sipStart, sipEnd).foldLeft(0.0):
          case (acc, fc @ FloatingCoupon[T](tFixing, tStart, tEnd, tPay)) =>
            val floatingDayCounter = rate.floatingRate.dayCounter
            val dcfStartEnd = DateLike[T].yearFraction(tStart, tEnd)(using floatingDayCounter)
            val (ipStart, ipEnd) = ip(fc)
            val dcfIp = DateLike[T].yearFraction(ipStart, ipEnd)(using floatingDayCounter)
            val pDiscTPay =
              factRepr.forwardZeroCoupon(tExpiry, tPay, rate.discountCurve, vAtExpiry)
            val pResTStart =
              factRepr.forwardZeroCoupon(tExpiry, ipStart, rate.floatingRate.resetCurve, vAtExpiry)
            val pResTEnd =
              factRepr.forwardZeroCoupon(tExpiry, ipEnd, rate.floatingRate.resetCurve, vAtExpiry)
            acc +
              dcfStartEnd / dcfIp * (
                E(pDiscTPay.mul(pResTStart).div(pResTEnd), xStar, vAtExpiry, exSign) -
                  E(pDiscTPay, xStar, vAtExpiry, exSign)
              )
      sign * discFixing * (valueFloating - swaption.strike * valueFixed)

  def price(swaption: Swaption[T]): Either[lib.Error, Double] =
    val measure = Measure(swaption.fixingAt, swaption.rate.currency)
    val factRepr = new HW1FactRepr[T](t0, lambda, sigma, measure)
    tvSwaption(swaption, factRepr.variance(swaption.fixingAt), factRepr)
